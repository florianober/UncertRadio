#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import xml.etree.ElementTree as ET


def extract_widget_info(glade_file):
    tree = ET.parse(glade_file)
    root = tree.getroot()

    # Dictionary to store parent ID information
    parent_map = {}

    # Populate the parent_map with parent-child relationships
    for parent in root.findall('.//object'):
        for child in parent.findall('child/object'):
            parent_map[child] = parent

    widget_info = []

    for widget in root.findall('.//object'):
        widget_id = widget.get('id')
        widget_class = widget.get('class')

        # Get the parent of the current widget
        parent = parent_map.get(widget)

        # Get the ID of the parent if it exists
        parent_id = parent.get('id') if parent is not None else "None"

        if widget_id and widget_class:
            widget_info.append({
                'id': widget_id,
                'class': widget_class,
                'parent_id': parent_id
            })

    return widget_info


def generate_fortran_widget_module(widget_info, output_file):
    """Generate a Fortran module file from widget IDs."""

    with open(output_file, 'w') as f:

        f.write("!-------------------------------------------------------------------------------------------------!\n")
        f.write("! This file is part of UncertRadio and is automatically generated.\n")
        f.write("! Do not edit!\n")
        f.write("!-------------------------------------------------------------------------------------------------!\n")
        f.write("\n")
        f.write("module glade_widgets_auto \n")

        f.write("    use UR_types, only: widget_type\n")

        f.write("    implicit none\n")
        f.write("\n")
        f.write("    type :: widgets_named\n")

        f.write(f"        type(widget_type) :: widget_array(1:{len(widget_info)})\n")
        f.write("        type(widget_type), pointer :: window1 => null()\n")
        f.write("    end type\n")
        f.write("\n")

        f.write("    !---------------------------------------------------------------------------------------------!\n")
        f.write("\n")
        f.write("contains\n")
        f.write("\n")

        f.write("    function initialize_single_widget(classname, gladeid, gladeid_parent) result(widget) \n")
        f.write("        implicit none\n")
        f.write("        type(widget_type) :: widget\n")
        f.write("        character(len=*), intent(in) :: gladeid, classname, gladeid_parent\n")
        f.write("        !-----------------------------------------------------------------------------------------!\n")
        f.write("\n")
        f.write("        widget%classname = classname\n")
        f.write("        widget%gladeid = gladeid\n")
        f.write("        widget%gladeid_parent = gladeid_parent\n")
        f.write("    end function initialize_single_widget\n")
        f.write("\n")

        f.write("    subroutine initialize_widgets(widgets)\n")
        f.write("        implicit none\n")
        f.write("        type(widgets_named), intent(inout) :: widgets\n")
        f.write("        !-----------------------------------------------------------------------------------------!\n")
        for i, widget in enumerate(widget_info):

            f.write(f"        widgets%widget_array({i+1}) = initialize_single_widget('{widget['id']}', '{widget['class']}', '{widget['parent_id']}')\n")
            f.write("\n")

        f.write("    end subroutine initialize_widgets\n")
        f.write("    !---------------------------------------------------------------------------------------------!\n")
        f.write("end module \n")


if __name__ == "__main__":
    glade_file = 'UR2_5.glade'  # Replace with your Glade file path
    output_file = 'src/UR/glade_widgets_auto.f90'   # Output Fortran include file

    widget_info = extract_widget_info(glade_file)

    for widget in widget_info:
        if widget['id'] == 'cellrenderertext91':
            print(widget['id'])
            print(widget['class'])
            print(widget['parent_id'])

    generate_fortran_widget_module(widget_info, output_file)
