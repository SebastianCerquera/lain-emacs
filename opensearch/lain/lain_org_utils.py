import os
import re
import datetime
from abc import ABC, abstractmethod
from typing import List, Optional

import orgparse

# Base Classes
class OrgEntity(ABC):
    @abstractmethod
    def accept(self, visitor):
        pass

class OrgFileComponent(OrgEntity):
    pass

class OrgTaskComponent(OrgEntity):
    pass

class OrgThreadComponent(OrgEntity):
    pass

# Concrete Classes
class OrgFile(OrgFileComponent):
    def __init__(self, root: OrgTaskComponent, tasks: List[OrgTaskComponent]):
        self.tasks = tasks
        self.root = root

        self.tasks.append(root)

    def accept(self, visitor):
        visitor.visit_org_file(self)

        self.root.accept(visitor)


class OrgTask(OrgTaskComponent):
    def __init__(self, node: orgparse.node.OrgNode, title: Optional[str] = None, 
                 parent: Optional['OrgTask'] = None, 
                 threads: List['OrgThreadComponent'] = None):
        self.org_node = node
        self.title = title
        self.parent = parent
        self.children = []
        self.threads = threads if threads else []

    def add_child(self, child: 'OrgTask'):
        child.parent = self
        self.children.append(child)

    def add_thread(self, thread: 'OrgThreadComponent'):
        self.threads.append(thread)

    def accept(self, visitor):
        visitor.visit_org_task(self)
        for thread in self.threads:
            thread.accept(visitor)
        for child in self.children:
            child.accept(visitor)

class OrgThread(OrgThreadComponent):
    @staticmethod
    def parse(raw: str) -> OrgThreadComponent:
        return OrgThread(raw)
    
    def __init__(self, content: str, parent: Optional['OrgThread'] = None, 
                 timestamp: Optional[datetime.datetime] = None):
        self.raw = content
        self.parent = parent
        self.timestamp = timestamp
        self.children = []
        self.content = None

    def add_child(self, child: 'OrgThread'):
        child.parent = self
        self.children.append(child)

    def accept(self, visitor):
        visitor.visit_org_thread(self)
        for child in self.children:
            child.accept(visitor)

# Visitors
class OrgVisitor(ABC):
    @abstractmethod
    def visit_org_file(self, org_file: OrgFileComponent):
        pass

    @abstractmethod
    def visit_org_task(self, org_task: OrgTaskComponent):
        pass

    @abstractmethod
    def visit_org_thread(self, org_thread: OrgThreadComponent):
        pass

# Features
class OrgFileDiscovery:
    @staticmethod
    def discover_files(directory: str) -> List[str]:
        return [os.path.join(directory, f) for f in os.listdir(directory) if f.endswith('.org')]
    
class OrgDatabase(OrgVisitor):

    def visit_org_file(self, org_file: OrgFile):
        print(f"Persisting org file: {org_file.root.title}")

    def visit_org_task(self, task: OrgTask):
        print(f"Persisting org task: {task.title}")

    def visit_org_thread(self, thread: OrgThread):
        print(f"Persisting org thread: {thread.timestamp}")
    
class OrgParser:

    @staticmethod
    def _parse_tasks(node: orgparse.node.OrgNode, parent: Optional[OrgTask] = None) -> List[OrgTask]:
        if node is None:
            return []

        task = OrgTask(node)

        if parent:
            task.parent = parent
            parent.add_child(task)

        if len(node.children) == 0:
            return [task]
        
        return [task] + [nested_task for child in node.children
                    for nested_task in OrgParser._parse_tasks(child, parent=task)]

    @staticmethod
    def parse(file_path: str) -> OrgFile:
        org_tree = orgparse.load(file_path)

        tasks = OrgParser._parse_tasks(org_tree.children[0])
        org_file = OrgFile(tasks[0], tasks[1:])

        org_file.accept(OrgParserVisitor())
        org_file.accept(CleaningVisitor())

        org_file.accept(OrgDatabase())

        return org_file


class OrgParserVisitor(OrgVisitor):
 
    def visit_org_file(self, org_file: OrgFile):
        pass

    def visit_org_task(self, task: OrgTask):
        if task.org_node.body is None:
            return
        
        lines = task.org_node.body.split('\n')

        for i in range(len(lines)):
            if lines[i] != '':
                task.add_thread(OrgThread("\n".join(lines[i:])))

    def visit_org_thread(self, thread: OrgThread):
        pass

# Visitor Implementations
class CleaningVisitor(OrgVisitor):
    TIMESTAMP_REGEX = r'- <(\d{4}-\d{2}-\d{2})>'

    def visit_org_file(self, org_file: OrgFile):
        pass

    def visit_org_task(self, org_task: OrgTask):
        org_task.title = org_task.org_node.heading

    def visit_org_thread(self, org_thread: OrgThread):
        self._extract_and_set_timestamp(org_thread)
        self._clean_content(org_thread)
        if not org_thread.timestamp:
            self._set_lowest_timestamp(org_thread)

    def _clean_content(self, org_thread: OrgThread):
        org_thread.content = re.sub(self.TIMESTAMP_REGEX, '', org_thread.raw.split('\n')[0]).strip()

    def _extract_and_set_timestamp(self, org_thread: OrgThread):
        match = re.search(self.TIMESTAMP_REGEX, org_thread.raw)
        if match:
            org_thread.timestamp = datetime.datetime.strptime(match.group(1), '%Y-%m-%d')

    def _set_lowest_timestamp(self, org_thread: OrgThread):
        current = org_thread
        while current.parent and not current.timestamp:
            current = current.parent
        org_thread.timestamp = current.timestamp

class OrgModule:
    def run(self):
        files = OrgFileDiscovery.discover_files("sample_files")
        for file_path in files:
            OrgParser.parse(file_path)