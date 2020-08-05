#!/usr/bin/env python

from orgparse import load, loads
import jinja2
import os

def render_template(projects, template_file):
    """Renders a template with the website's structure.
    """
    templateLoader = jinja2.FileSystemLoader(searchpath="./")
    templateEnv = jinja2.Environment(loader=templateLoader)
    template = templateEnv.get_template(template_file)
    return template.render(projects=projects)

def write_file(text, path):
    """Writes a string to a file.
    """
    f = open(path, "w+")
    f.write(text)

def parse_projects(project_path):
    """Parses description information from README submodules.
    """
    projects = []
    for project_name in os.listdir('src/projects/'):
        path = f'{project_path}/{project_name}/README.org'
        root = load(path)
        for heading_node in root.children:
            if heading_node.get_property('PROJECT_DESCRIPTION') is not None:
                projects.append({
                    'title': project_name,
                    'description': heading_node.body
                })
                break
    return projects 


parsed_projects = parse_projects('src/projects')
write_file(render_template(parsed_projects, "./src/templates/projects.html"), "./public/projects/index.html")
