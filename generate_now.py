#!/usr/bin/env python

from orgparse import load, loads
import jinja2

def render_template(site, template_file):
    """Renders a template with the website's structure.
    """
    templateLoader = jinja2.FileSystemLoader(searchpath="./")
    templateEnv = jinja2.Environment(loader=templateLoader)
    template = templateEnv.get_template(template_file)
    return template.render(site=site)

def parse_list(list_text):
    """Parse a list of items from Org-mode text.
    The list follows the following structure:
    - Item 1
    - Item 2
    - Item 3
    Assumes no sublists.
    """
    # NOTE: Currently ignores non-bulleted lines.
    def parse_line(line):
        if line[0:2] == '- ':
            return {'li': line[2:]}
        elif line[0:2] == '#+':
            pass
        else:
            if line != '':
                return {'line': line}
        
    return list(filter(None,
                       list(map(parse_line,
                                list_text.split("\n")))))

def parse_links(line):
    """Parses org-mode format links from a line of text.
    """
    text = []
    plaintext = ""

    i = 0
    while i < len(line):
        if line[i] == '[':
            text.append({
                'text': plaintext
            })
            i += 1
            link = line[i:].substring(0 + line[i:].index(']'))
            i += len(link) + 1
            text = line[i:].substring(0 + line[i:].index(']]'))
            i += len(text) + 2
            text.append({
                'link': link,
                'text': text
            })
        else:
            plaintext += line[i]
            i += 1

    text.append({
        'text': plaintext
    })

    return text
            

def parse_file(path):
    """Generates a simple representation of an org-mode file.
    """
    # GitHub README <h1/> for Org is too big, so we start at <h2/>
    root = load(path).children[0]
    site_text = {
        'title': root.heading,
        'subheadings': []
    }
    for heading_node in root.children:
        site_text['subheadings'].append({
            'title': heading_node.heading,
            'body': parse_list(heading_node.body)
        })
    return site_text

def write_file(text, path):
    """Writes a string to a file.
    """
    f = open(path, "w+")
    f.write(text)

parsed_file = parse_file('src/jakechv/README.org')
# print(parsed_file)
write_file(render_template(parsed_file, "./src/templates/now.html"), "./public/now/index.html")
