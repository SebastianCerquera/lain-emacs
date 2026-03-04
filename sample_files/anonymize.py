import re
import random
from datetime import datetime, timedelta

def generate_lorem_ipsum(word_count):
    lorem_words = "lorem ipsum dolor sit amet consectetur adipiscing elit sed do eiusmod tempor incididunt ut labore et dolore magna aliqua".split()
    return ' '.join(random.choices(lorem_words, k=word_count))

def shuffle_time_logs(line):
    match = re.search(r'CLOCK: \[.*?]\[.*?]\] =>\s*(\d+):(\d+)', line)
    if not match:
        return line

    new_total_minutes = random.randint(1, 8 * 60)
    new_hours = new_total_minutes // 60
    new_minutes = new_total_minutes % 60

    start_time = datetime.now() - timedelta(days=random.randint(0, 30),
                                            hours=random.randint(0, 23),
                                            minutes=random.randint(0, 59))
    end_time = start_time + timedelta(minutes=new_total_minutes)

    day_abbrs_es = ['lun', 'mar', 'mié', 'jue', 'vie', 'sáb', 'dom']
    start_day_abbr = day_abbrs_es[start_time.weekday()]
    end_day_abbr = day_abbrs_es[end_time.weekday()]
    
    start_str = f"{start_time.strftime('%Y-%m-%d')} {start_day_abbr} {start_time.strftime('%H:%M')}"
    end_str = f"{end_time.strftime('%Y-%m-%d')} {end_day_abbr} {end_time.strftime('%H:%M')}"

    return f"    CLOCK: [{start_str}]--[{end_str}] =>  {new_hours}:{new_minutes:02d}"


def anonymize_org_file(content):
    lines = content.split('\n')
    new_lines = []
    
    header_regex = re.compile(r'^(\*+)\s+(TODO|IN_PROGRESS|CHECK|DONE|NO_STATE|LATER|CANCELED)?\s*(\[#[A-Z]\])?(.*)')
    content_line_regex = re.compile(r'^\s*-\s+<.*?>\s+(.*)')
    link_regex = re.compile(r'\[\[.*?]\[(.*?)]\]') 
    simple_link_regex = re.compile(r'\[\[(.*?)]]') 

    name_map = {}
    keywords = ['RESPONSABILITIES']
    original_categories = []
    
    # First pass: find all names and categories from header parts
    for line in lines:
        header_match = header_regex.match(line)
        if header_match:
            stars, _, _, text = header_match.groups()
            tags_match = re.search(r'(\s*:[a-zA-Z0-9_@]+:)+$', text)
            text_without_tags = text.replace(tags_match.group(0), '') if tags_match else text
            parts = [p.strip() for p in text_without_tags.strip().split(',')]
            
            if len(stars) == 2 and parts[0] and parts[0] not in original_categories:
                original_categories.append(parts[0])

            for part in parts:
                if part and not part.isdigit() and not re.match(r'\d{4}-\d{2}', part) and part not in keywords:
                    if part not in name_map:
                        name_map[part] = generate_lorem_ipsum(1)

    # Second pass: transform with context
    context_stack = [{'level': 0}] # Root context

    for line in lines:
        if any(line.strip().startswith(p) for p in ['#', ':LOGBOOK:', ':END:', ':PROPERTIES:', 'CLOSED:', '|', '#+']):
            new_lines.append(line)
            continue

        if 'CLOCK:' in line:
            new_lines.append(shuffle_time_logs(line))
            continue

        header_match = header_regex.match(line)
        if header_match:
            stars, state, priority, text = header_match.groups()
            level = len(stars)

            while context_stack[-1]['level'] >= level:
                context_stack.pop()
            
            parent_context = context_stack[-1]
            current_context = parent_context.copy()
            current_context['level'] = level

            tags_match = re.search(r'(\s*:[a-zA-Z0-9_@]+:)+$', text)
            tags = tags_match.group(0) if tags_match else ''
            text_without_tags = text.replace(tags, '').strip()
            parts = [p.strip() for p in text_without_tags.strip().split(',')]

            if not parts or not parts[0]:
                new_lines.append(line)
                continue

            # Determine context for this level and children
            if level == 2:
                current_context['cat'] = name_map.get(parts[0])
            
            has_date = any(re.match(r'\d{4}-\d{2}', p) for p in parts)
            has_num = any(p.isdigit() and len(p) < 3 for p in parts)

            if has_date and has_num and 'date' not in parent_context:
                current_context['date'] = f"{random.randint(2020, 2025)}-{random.randint(1, 12):02d}"
                current_context['num'] = str(random.randint(1, 5))
            
            context_stack.append(current_context)

            # Now build the new header
            new_parts = []
            for part in parts:
                if part in name_map: 
                    new_parts.append(name_map[part])
                elif part in original_categories and 'cat' in current_context: 
                    new_parts.append(current_context['cat'])
                elif re.match(r'\d{4}-\d{2}', part) and 'date' in current_context: 
                    new_parts.append(current_context['date'])
                elif part.isdigit() and len(part) < 3 and 'num' in current_context: 
                    new_parts.append(current_context['num'])
                else: 
                    new_parts.append(part)

            new_text = ', '.join(new_parts)
            
            new_header = stars
            if state: new_header += f' {state}'
            if priority: new_header += f' {priority}'
            new_header += f' {new_text}'
            if tags: new_header += f'{tags}'
            
            new_lines.append(new_header)
            continue

        content_match = content_line_regex.match(line)
        if content_match:
            new_lines.append(line.split('>')[0] + '> ' + generate_lorem_ipsum(random.randint(8, 20)))
            continue
        
        line = link_regex.sub(lambda m: f"[[{m.group(1)}][{generate_lorem_ipsum(2)}]]", line)
        line = simple_link_regex.sub(lambda m: f"[[{generate_lorem_ipsum(2)}]]", line)

        if line.strip():
             new_lines.append(' ' * (len(line) - len(line.lstrip(' '))) + generate_lorem_ipsum(random.randint(5, 15)))
        else:
            new_lines.append(line)

    return '\n'.join(new_lines)

with open('scrum.org', 'r') as f:
    original_content = f.read()

anonymized_content = anonymize_org_file(original_content)

with open('scrum.obfuscated.org', 'w') as f:
    f.write(anonymized_content)

print("Anonymized file 'scrum.obfuscated.org' created successfully.")