import logging
import os
import re
import datetime

import string
import random

from opensearchpy import OpenSearch

from abc import ABC, abstractmethod
from typing import List, Optional

import orgparse

from dotenv import load_dotenv

load_dotenv()

logger = logging.getLogger(__name__)

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
        raise NotImplementedError()

    def accept(self, visitor):
        visitor.visit_org_file(self)

        self.root.accept(visitor)


class OrgTask(OrgTaskComponent):

    def __init__(self, node: orgparse.node.OrgNode, parent: Optional['OrgTask'] = None, 
                 threads: List['OrgThreadComponent'] = None):
        self.org_node = node
        self.title = node.heading
        self.parent = parent
        self.children = []
        self.threads = threads if threads else []

    def add_child(self, child: 'OrgTask'):
        child.parent = self
        self.children.append(child)

    def add_thread(self, thread: 'OrgThreadComponent'):
        self.threads.append(thread)

    def to_json(self):
        raise NotImplementedError()

    def accept(self, visitor):
        visitor.visit_org_task(self)

        for child in self.children:
            child.accept(visitor)

        for thread in self.threads:
            thread.accept(visitor)

class OrgThread(OrgThreadComponent):
    
    def __init__(self, raw: str, task: OrgTask,  
                 parent: Optional['OrgThread'] = None, 
                 timestamp: Optional[datetime.datetime] = None,
                 content: Optional[str] = None):
        self.raw = raw
        self.parent = parent
        self.timestamp = timestamp
        self.children = []
        self.content = content

        self.task = task

    def add_child(self, child: 'OrgThread'):
        child.parent = self
        self.children.append(child)

    def to_json(self):
        return {
            'thread_date': self.timestamp,
            "thread_body": self.content,
            "task_id": self.task.id
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
              "filter": {
                "english_stop": {
                  "type":       "stop",
                  "stopwords":  "_english_" 
                },
                "english_stemmer": {
                  "type":       "stemmer",
                  "language":   "english"
                },
                "spanish_stop": {
                  "type":       "stop",
                  "stopwords":  "_spanish_" 
                },
                "my_custom_stop_words_filter": {
                  "type": "stop",
                  "ignore_case": "true",
                  "stopwords": [ 
                      "En",
                      "no",
                      "No",
                      "el",
                      "El",
                      "la",
                      "La"
                      ]
                }
              },
              "analyzer": {
                "ma": {
                  "tokenizer": "mt",
                  "filter": [
                    "english_stop",
                    "spanish_stop",
                    "my_custom_stop_words_filter",
                    "lowercase",
                    "asciifolding",
                    "classic",
                    "delimited_payload"
                  ]
                }
              },
              "tokenizer": {
                "mt": {
                  "type": "char_group",
                  "tokenize_on_chars": [
                    "whitespace",
                    ",",
                    ".",
                    ";",
                    "\n",
                    "?",
                    "[",
                    "]"
                  ]
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
                "keyword": {
                  "type": "keyword",
                  "ignore_above": 32766
                }, 
            "task_id": {
              "type": "text",
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
        logger.debug("Initializing OrgDatabase with index_name: %s", index_name)
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
        logger.debug("Visiting org thread: %s", thread.to_json())
        if "\end{verbatim" in thread.content:
            return

        try:
            self.elasticsearch.index(index=self.index_name, body=thread.to_json())
        except Exception as e:
            logger.error("Failed to index thread: %s", thread.to_json(), exc_info=True)

class ThreadParser():

    ORG_BULLET_PATTERN = r'-?\s*<(\d{4}-\d{2}-\d{2}).{0,5}>\s(.*)'

    ORG_LOG_PATTERN = r'\sState\s"(\w+)"'

    @staticmethod
    def parse_raw(raw: str, task: OrgTask, is_root=False) -> List[OrgThread]: 
        raw = raw.replace("\t", "    ")
        indentation_rule = raw.find("- ")

        if indentation_rule == -1:
            return

        if indentation_rule == 0:
            bullets = raw[indentation_rule+1:].split("\n-")
        elif indentation_rule == 1:
            bullets = raw[indentation_rule+1:].split("\n -")
        else:
            if raw[:indentation_rule].rfind("\n") == -1:
                bullets = ("\n" + raw).split("\n" + raw[:indentation_rule+1])
            else:
                new_line = raw[:indentation_rule].rfind("\n")
                bullets = raw[new_line:].split(raw[new_line:indentation_rule+1])

        if not is_root:
            bullets = [raw[:indentation_rule]] + bullets

        if len(bullets) == 0:
            return

        # This was to filter out empty strings produced by the split
        bullets = list(filter(lambda x: x != '', bullets))
        bullets = list(filter(lambda x: not re.match(r'\s+$', x), bullets))

        # This was to remove the org log checks on periodic tasks
        bullets = list(filter(lambda x: re.match(ThreadParser.ORG_LOG_PATTERN, x) is None, bullets))

        threads = []
        for bullet in bullets:
            thread = OrgThread("- " + bullet.strip(), task)

            if is_root:
                task.add_thread(thread)

            threads.append(thread)

        return threads
    
    @staticmethod
    def _check_code_block(thread: OrgThread, content: str) -> bool:
        start_block = content.find("\\begin{verbatim}")

        if start_block == -1:
            return False 
        
        if start_block == 0:
            return True

        thread.content = content[:start_block].strip()
        
        end_block = content.find("\\end{verbatim}")

        thread.add_child(OrgThread("- " + content[start_block:end_block+13], thread.task, parent=thread))

        return True

    @staticmethod
    def parse_thread(thread: OrgThread):
         date_text = re.match(ThreadParser.ORG_BULLET_PATTERN, thread.raw, flags=re.DOTALL)
        
         if date_text:
            thread.timestamp = datetime.datetime.strptime(date_text.group(1), '%Y-%m-%d').date()

            body = date_text.group(2)
            next_bullet = body.find("-")

            new_line = body[:next_bullet].rfind("\n")

            subthreads = ThreadParser.parse_raw(
                body[next_bullet:] if new_line == -1 else body[new_line:], thread.task)
            
            if not subthreads:
                thread.content = body.strip()
            else:
                thread.content = body[:next_bullet].strip()

            ThreadParser._check_code_block(thread, thread.content)
            
            if not subthreads:
                return

            for subthread in subthreads:
                thread.add_child(subthread)
         else:
            next_bullet = thread.raw.find("-")

            subthreads = ThreadParser.parse_raw(thread.raw[next_bullet+1:], thread.task)

            if not subthreads:
                thread.content = thread.raw[next_bullet+1:].strip()

                ThreadParser._check_code_block(thread, thread.content)

                thread.content = None if thread.content == '' else thread.content

            else:
                list_index = subthreads[0].raw.find("- ")
                thread.content = subthreads[0].raw[list_index+2:].strip() if list_index != -1 else subthreads[0].raw.strip()

                ThreadParser._check_code_block(thread, thread.content)
                
                for subthread in subthreads[1:]:
                    thread.add_child(subthread)

            if thread.content:
                thread.timestamp = datetime.datetime.now().date()

class OrgParserVisitor(OrgVisitor):

    def visit_org_file(self, org_file: OrgFile):
        pass

    def visit_org_task(self, task: OrgTask):
        if task.org_node.body is None:
            return
        
        if task.org_node.body == '' or re.match(r'[\s\t\n]*$', task.org_node.body):
            return
        
        ThreadParser.parse_raw(task.org_node.get_body(format="raw"), task, is_root=True)
        

    def visit_org_thread(self, thread: OrgThread):
        ThreadParser.parse_thread(thread)


# Visitor Implementations
class CleaningVisitor(OrgVisitor):
    HTTP_REGEX = r".*(https?://[^\s]+)"

    TIMESTAMP_REGEX = r'- <(\d{4}-\d{2}-\d{2})[\w\s]*>'

    links = {}

    def visit_org_file(self, org_file: OrgFile):
        pass

    def visit_org_task(self, org_task: OrgTask):
        org_task.title = org_task.org_node.heading 
        org_task.id = f"TASKID{''.join(random.choices(string.printable[:62], k=len(org_task.title)))}"
        self.links[org_task.title] = org_task.id

    def visit_org_thread(self, org_thread: OrgThread):
        self._extract_and_set_timestamp(org_thread)
        if not org_thread.timestamp:
            self._set_lowest_timestamp(org_thread)
        self._clean_content(org_thread)
        self._check_org_links(org_thread)
        self._check_html_links(org_thread)
        self._clean_indentation(org_thread)

    def _check_html_links(self, thread: OrgThread):
        if thread.content is None:
            return
        
        link = re.match(self.HTTP_REGEX, thread.content, flags=re.DOTALL)
        if link:
            self.links[link.group(1)] = f"HTTPID{''.join(random.choices(string.printable[:62], k=len(link.group(1))))}"
            thread.content = thread.content.replace(link.group(1), self.links[link.group(1)])

    def _check_org_links(self, thread: OrgThread):
        if thread.content is None:
            return
        
        link = re.match(r'\[\[(.+)\]\[(.+)\]\]', thread.content, flags=re.DOTALL) 
        if link:
            if not link.group(1) in self.links:
                self.links[link.group(1)] = f"TASKID{''.join(random.choices(string.printable[:62], k=len(link.group(1))))}"
            thread.content = re.sub(r'\[\[(.+)\]\[(.+)\]\]', f"[[{self.links[link.group(1)]}][{link.group(2)}]]", thread.content, flags=re.DOTALL).strip()
            return

        link = re.match(r'\[\[(.+)\]\]', thread.content) 
        if link:
            if not link.group(1) in self.links:
                self.links[link.group(1)] = f"TASKID{''.join(random.choices(string.printable[:62], k=len(link.group(1))))}"
            thread.content = re.sub(r'\[\[(.+)\]\]', self.links[link.group(1)], thread.content, flags=re.DOTALL).strip()

    def _clean_content(self, org_thread: OrgThread):
        if org_thread.content:
            return
        
        if re.match(self.TIMESTAMP_REGEX, org_thread.raw):
            org_thread.content = re.sub(self.TIMESTAMP_REGEX, '', org_thread.raw).strip()
        else:
            org_thread.content = re.sub(r'^-', '', org_thread.raw).strip()

    def _clean_indentation(self, org_thread: OrgThread):
        if not org_thread.content:
            return

        org_thread.content = " ".join(map(lambda e: e.strip(), org_thread.content.split("\n")))

    def _extract_and_set_timestamp(self, org_thread: OrgThread):
        match = re.search(self.TIMESTAMP_REGEX, org_thread.raw)
        if match:
            org_thread.timestamp = datetime.datetime.strptime(match.group(1), '%Y-%m-%d')

    def _set_lowest_timestamp(self, org_thread: OrgThread):
        current = org_thread
        while current.parent and not current.timestamp:
            current = current.parent

        if current.timestamp:
            org_thread.timestamp = current.timestamp
        else:
            org_thread.timestamp = datetime.datetime.now().date()



class OrgThreadContentCollector(OrgVisitor):
    def __init__(self):
        self.thread_contents = set()

    def visit_org_file(self, org_file: OrgFileComponent):
        pass

    def visit_org_task(self, org_task: OrgTaskComponent):
        pass

    def visit_org_thread(self, org_thread: OrgThreadComponent):
        if org_thread.content:
            self.thread_contents.add(org_thread.content)

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

        return org_file

class OrgFileDiscovery:
    @staticmethod
    def discover_files(directory: str) -> List[str]:
        return [os.path.join(directory, f) for f in os.listdir(directory) if f.endswith('.org')]

class OrgModule:
    def run(self, source_path: str):
        logger.debug("OrgModule.run called with source_path: %s", source_path)
        files = OrgFileDiscovery.discover_files(source_path)
        for file_path in files:
            OrgParser.parse(file_path).accept(OrgDatabase())
