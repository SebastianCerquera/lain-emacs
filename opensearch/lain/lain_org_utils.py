import os
import datetime
from abc import ABC, abstractmethod
from typing import List, Optional

from orgparse import load, loads

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
    def __init__(self, path: str, tasks: List['OrgFileComponent']):
        self.path = path
        self.tasks = tasks

    def accept(self, visitor):
        visitor.visit_org_file(self)

class OrgTask(OrgTaskComponent):
    def __init__(self, title: str, parent: Optional['OrgTask'] = None, threads: List['OrgThreadComponent'] = None):
        self.title = title
        self.parent = parent
        self.threads = threads if threads else []

    def add_child(self, child: 'OrgTask'):
        child.parent = self

    def add_thread(self, thread: 'OrgThreadComponent'):
        self.threads.append(thread)

    def accept(self, visitor):
        visitor.visit_org_task(self)
        for thread in self.threads:
            thread.accept(visitor)
        for child in self.get_children():
            child.accept(visitor)

    def get_children(self) -> List['OrgTask']:
        return [task for task in self.threads if task.parent == self]

class OrgThread(OrgThreadComponent):
    @staticmethod
    def parse(raw: str) -> OrgThreadComponent:
        return OrgThread(raw)
    
    def __init__(self, content: str, parent: Optional['OrgThread'] = None, 
                 timestamp: Optional[datetime.datetime] = None):
        self.content = content
        self.parent = parent
        self.timestamp = timestamp
        self.children = []

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

class OrgParser:
    @staticmethod
    def parse(file_path: str) -> OrgFile:
        org_tree = load(file_path)

        ## First recursion case
        task1 = OrgTask(org_tree.children[0].heading)
        thread1 = OrgThread(org_tree.children[0].body)
        task1.add_thread(thread1)

        ## 2nd recursion case
        task2 = OrgTask(org_tree.children[0].children[0].heading, parent=task1)
        thread2 = OrgThread(org_tree.children[0].children[0].body)
        task2.add_thread(thread2)

        task1.add_child(task2)

        return OrgFile(file_path, [task1, task2])

class OrgCleaner:
    @staticmethod
    def clean(org_thread: OrgThread):
        if not org_thread.timestamp and org_thread.parent:
            org_thread.timestamp = datetime.datetime.now()

class OrgDatabase:
    @staticmethod
    def persist(org_file: OrgFile):
        # Dummy implementation, replace with actual database logic
        print(f"Persisting org file: {org_file.path}")

class OrgModule:
    def run(self):
        files = OrgFileDiscovery.discover_files(".")
        for file_path in files:
            org_file = OrgParser.parse(file_path)
            self._process_org_file(org_file)

    def _process_org_file(self, org_file: OrgFile):
        org_file.accept(CleaningVisitor())
        OrgDatabase.persist(org_file)

# Visitor Implementations
class CleaningVisitor(OrgVisitor):
    def visit_org_file(self, org_file: OrgFile):
        pass

    def visit_org_task(self, org_task: OrgTask):
        pass

    def visit_org_thread(self, org_thread: OrgThread):
        OrgCleaner.clean(org_thread)
