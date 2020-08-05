#!/usr/bin/env python

from orgparse import load, loads
import jinja2

def render_template(site):
    """Renders a template with the website's structure.
    """
    templateLoader = jinja2.FileSystemLoader(searchpath="./")
    templateEnv = jinja2.Environment(loader=templateLoader)
    TEMPLATE_FILE = "now.html"
    template = templateEnv.get_template(TEMPLATE_FILE)
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
    return list(filter(None,
                       list(map(lambda line: line[2:] if line[0:2] == '- ' else '',
                                list_text.split("\n")))))

def parse_file(path, no_render_prop_name='DoNotRender'):
    """Generates a simple representation of an org-mode file.
    """
    # GitHub README <h1/> for Org is too big, so we start at <h2/>
    root = load(path).children[0]
    site_text = {
        'title': root.heading,
        'subheadings': []
    }
    for heading_node in root.children:
        # if heading_node.get_property(no_render_prop_name) is None:
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
write_file(render_template(parsed_file), "./public/now/index.html")
