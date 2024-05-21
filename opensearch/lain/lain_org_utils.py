import os
import re
import datetime

from opensearchpy import OpenSearch

from abc import ABC, abstractmethod
from typing import List, Optional

import orgparse

from dotenv import load_dotenv

load_dotenv()

# Base Classes
class OrgEntity(ABC):

    @abstractmethod
    def accept(self, visitor):
        pass

    @abstractmethod
    def to_json(self):
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

    def to_json(self):
        return {
            'file_path': "Sample file path",
        }

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

    def to_json(self):
        return {
            'task_title': self.title
        }

    def accept(self, visitor):
        visitor.visit_org_task(self)
        for thread in self.threads:
            thread.accept(visitor)
        for child in self.children:
            child.accept(visitor)

class OrgThread(OrgThreadComponent):
    
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

    def to_json(self):
        return {
            'thread_date': self.timestamp,
            "thread_body": self.content
        }

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
class OrgDatabase(OrgVisitor):

    index_settings = {
      "settings": {
          "index": {
            "number_of_shards": "1",
            "analysis": {
              "analyzer": {
                "ma": {
                  "tokenizer": "mt"
                }
              },
              "tokenizer": {
                "mt": {
                  "type": "whitespace"
                }
              }
            },
            "number_of_replicas": "1"
          }
        },
      "mappings": {
          "properties": {
            "task_title": {
              "type": "text",
              "term_vector": "yes",
              "analyzer" : "ma",
              "fields": {
                "keyword": {
                  "type": "keyword",
                  "ignore_above": 256
                }
              }
            },
            "file_path": {
              "type": "text",
              "fields": {
                "keyword": {
                  "type": "keyword",
                  "ignore_above": 256
                }
              }
            },
            "thread_body": {
              "type": "text",
              "term_vector": "yes",
              "analyzer" : "ma",
              "fields": {
                "keyword": {
                  "type": "keyword",
                  "ignore_above": 256
                }
              }
            }, 
            "thread_date": {
              "type": "date",
            }
          }
        }
      }
    

    
    def __init__(self, index_name: str = 'my-org-index-2024-05-21--1'):
        endpoint = os.getenv("OPENSEARCH_ENDPOINT")

        self.index_name = index_name
        self.elasticsearch = OpenSearch(endpoint, verify_certs=False)

        if not self.elasticsearch.indices.exists(index=index_name):
            self.elasticsearch.indices.create(index=index_name, body=self.index_settings)
        
    def visit_org_file(self, org_file: OrgFile):
        pass

    def visit_org_task(self, task: OrgTask):
        pass

    def visit_org_thread(self, thread: OrgThread):
        self.elasticsearch.index(index=self.index_name, body=thread.to_json())


class OrgParserVisitor(OrgVisitor):

    LAIN_ENTRY_PATTERN = r'- <?(\d{4}-\d{2}-\d{2})?[\w\s]*>?(.*)'
 
    def visit_org_file(self, org_file: OrgFile):
        pass

    def visit_org_task(self, task: OrgTask):
        if task.org_node.body is None:
            return
        
        if task.org_node.body == '' or re.match(r'[\s\t]*$', task.org_node.body):
            return

        if re.match(r'[\n]*$', task.org_node.body):
            return
         
        lines = re.findall(self.LAIN_ENTRY_PATTERN, task.org_node.body)
        if len(lines) == 0:
            return

        parent = OrgThread("\n".join(map(lambda e: f"- <{e[0]}> {e[1].strip()}", lines)))
        task.add_thread(parent)

        for i in range(1, len(lines)):
            thread = OrgThread("\n".join(map(lambda e: f"- <{e[0]}> {e[1].strip()}", lines[i:])))
            parent.add_child(thread)

            task.add_thread(thread)

    def visit_org_thread(self, thread: OrgThread):
        pass

# Visitor Implementations
class CleaningVisitor(OrgVisitor):
    TIMESTAMP_REGEX = r'- <(\d{4}-\d{2}-\d{2})[\w\s]*>'

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
        org_thread.content = re.sub(self.TIMESTAMP_REGEX, '', org_thread.raw).strip()

    def _extract_and_set_timestamp(self, org_thread: OrgThread):
        match = re.search(self.TIMESTAMP_REGEX, org_thread.raw)
        if match:
            org_thread.timestamp = datetime.datetime.strptime(match.group(1), '%Y-%m-%d')

    def _set_lowest_timestamp(self, org_thread: OrgThread):
        current = org_thread
        while current.parent and not current.timestamp:
            current = current.parent
        org_thread.timestamp = current.timestamp



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

class OrgFileDiscovery:
    @staticmethod
    def discover_files(directory: str) -> List[str]:
        return [os.path.join(directory, f) for f in os.listdir(directory) if f.endswith('.org')]

class OrgModule:
    def run(self):
        files = OrgFileDiscovery.discover_files("sample_files")
        for file_path in files:
            OrgParser.parse(file_path)