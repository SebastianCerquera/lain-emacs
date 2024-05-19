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

    def accept(self, visitor):
        visitor.visit_org_file(self)

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
    
class OrgDatabase:
    @staticmethod
    def persist(org_file: OrgFile):
        # Dummy implementation, replace with actual database logic
        print(f"Persisting org file: {org_file.root.title}")

class OrgParser:

    @staticmethod
    def parse(file_path: str) -> OrgFile:
        org_tree = orgparse.load(file_path)

        root = OrgTask(org_tree.children[0])
        root.accept(CleaningVisitor())
        thread = OrgThread(root.org_node.body)
        root.threads = OrgParser._parse_thread(thread)

        tasks = [root]
        for node in org_tree.children[0].children:
            tasks = tasks + OrgParser._parse_task(root, node)

        return OrgFile(root, tasks)
    
    @staticmethod
    def _parse_task(parent: OrgTask, node: orgparse.node.OrgNode) -> List[OrgTask]:
        task = OrgTask(node)
        task.accept(CleaningVisitor())

        task.parent = parent
        parent.add_child(task)

        thread = OrgThread(task.org_node.body)
        task.threads = OrgParser._parse_thread(thread)

        return [task] + [ nested_task for child in node.children 
                         for nested_task in OrgParser._parse_task(task, child)]
    
    @staticmethod
    def _parse_thread(thread: OrgThread) -> List[OrgThread]:
        thread.accept(CleaningVisitor())
        if thread.raw is None or thread.raw == '':
            return []
        
        lines = thread.raw.split('\n')
        if len(lines) == 1:
            return [thread]

        return [thread] + [ nested_thread for i in range(1, len(lines))
                         for nested_thread in OrgParser._parse_thread(OrgThread('\n'.join(lines[i:])))]

class OrgModule:
    def run(self):
        files = OrgFileDiscovery.discover_files("sample_files")
        for file_path in files:
            org_file = OrgParser.parse(file_path)
            self._process_org_file(org_file)

    def _process_org_file(self, org_file: OrgFile):
        org_file.accept(CleaningVisitor())
        OrgDatabase.persist(org_file)

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
