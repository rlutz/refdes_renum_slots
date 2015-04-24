#!/usr/bin/python
# refdes_renum_slots - renumber components in one or more gEDA schematics
# Copyright (C) 2010 Joshua E. Lansford
# Copyright (C) 2015 Roland Lutz
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software Foundation,
# Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

# Usage: refdes_renum_slots [OPTION]... SCHEMATIC...

import getopt, math, os, sys
from gettext import gettext as _

## Numbering offset for schematic pages (e.g., for page_skip = 100,
## the first resistor on the first page would be R101).
page_skip = 0

## Next free index for a given file number and refdes letter.
next_index = {}

## Return the non-number part of a refdes string (e.g., 'R?' -> 'R')

def find_refdes_prefix(refdes):
    return refdes.rstrip('0123456789?')

## Return the number part of a refdes string (e.g., 'R?' -> '?')

def find_refdes_postfix(refdes):
    number_start = len(refdes)
    while number_start > 0 and refdes[number_start - 1] in '0123456789?':
        number_start -= 1

    if number_start == len(refdes):
        return 0
    return int(refdes[number_start:])

## Check whether a character is valid for use in a refdes.

def is_refdes_char(test_char):
    return test_char.isalpha() or test_char.isdigit() or test_char in '?_'

def build_lookup_key_and_initialize(refdes, filenumber):
    refdes_prefix = find_refdes_prefix(refdes)

    if page_skip == 0:
        # all pages share a number namespace
        key = refdes_prefix
    else:
        # components on each page are numbered separately
        key = filenumber, refdes_prefix

    if key not in next_index:
        next_index[key] = (filenumber + 1) * page_skip + 1

    return key

## Return and post-increment the next free refdes number for the given
## file and refdes.

def refdes_count_obtain(refdes, filenumber):
    key = build_lookup_key_and_initialize(refdes, filenumber)

    answer = next_index[key]
    next_index[key] += 1
    return answer

## Make the available refdes numbers start after this component's number.
#
# Make sure the first renumbered component of the same type on this
# page gets assigned a higher number than this existing component.

def refdes_count_take_note_of(component):
    if '?' in component.refdes:
        return

    key = build_lookup_key_and_initialize(component.refdes,
                                          component.filenumber)

    refdes_postfix = find_refdes_postfix(component.refdes)
    if refdes_postfix >= next_index[key]:
        next_index[key] = refdes_postfix + 1


class Component:
    def __init__(self, device, refdes, filenumber, filename, slot,
                       is_partition, x, y, symbol):
        self.device = device			# str
        self.symbol = symbol			# str
        self.refdes = refdes			# str
        self.old_refdes = refdes		# str
        self.filenumber = filenumber		# int
        self.filename = filename		# str
        self.slot = slot			# str
        self.is_partition = is_partition	# bool
        self.currently_numbering = False	# bool
        self.x = x				# int
        self.y = y				# int
        self.group_buddies = []			# list of Component

    def refdes_is_updated(self):
        return self.refdes != self.old_refdes and '?' not in self.old_refdes

    # returns the distance in the schematic to the other component

    def distance_to(self, other_component):
        if self.filenumber == other_component.filenumber:
            return int(math.sqrt(pow(other_component.x - self.x, 2) +
                                 pow(other_component.y - self.y, 2)))
        else:
            return abs(self.filenumber - other_component.filenumber) * 1000000

    # This replaces the number portion of the refdes with a question mark

    def clear_refdes_number(self):
        prefix = find_refdes_prefix(self.refdes)
        if prefix != self.refdes:
            # don't add a question mark if there hasn't been a number
            self.refdes = prefix + '?'

    # Will cause this component to compute a refdes within context of
    # the components around it.  Will return a refdes with a question
    # mark if it gets called from a peer which it is calling.

    def refdes_number(self, components):
        if self.currently_numbering or '?' not in self.refdes:
            return self.refdes

        self.currently_numbering = True
        test_refdes = obtain_parent_refdes(self, components)
        if test_refdes is None:
            # get a new refdes and make sure it is unique
            is_unique = False
            while not is_unique:
                test_refdes = '%s%d' % (
                    self.refdes[:self.refdes.index('?')],
                    refdes_count_obtain(self.refdes, self.filenumber))
                is_unique = True
                for other_component in components:
                    if other_component.refdes == test_refdes:
                        is_unique = False
                        break
        self.refdes = test_refdes
        self.currently_numbering = False
        return self.refdes

    def to_string(self):
        return '%s %s %s,%s' % (self.refdes, self.device, self.x, self.y)

    # Returns a number used to sort components for numbering based off
    # of there position.

    def get_sort_height(self):
        return self.y * 10 - self.x

## Search through the components and try to find another component of
## the same device type which does not already has a free slot this
## component could use.
#
# \warning This function changes the order of the elements in \a components.
#
# \returns the found refdes, or \c None if no matching slot was found

def obtain_parent_refdes(child, components):
    if child.device == '':
        return None

    # sort the possible_parents in order of distance from this component
    components.sort(key = lambda o: o.distance_to(child))

    # check to see if child is part of a manual group.
    # if it is, then the parent has to be from the group

    if child.group_buddies or child.slot == '':
        # If we are part of a manual group then we should just iterate
        # over all the other components in the group and if any of them
        # have their refdes yet, then that will be our refdes.  If
        # none of them do, then we don't know yet ourselves either.
        for possible_parent in child.group_buddies:
            # Don't take a component as a parent which isn't one of the
            # processed components yet.  If we don't, components do not get
            # numbered in the correct order with respect to where they
            # are on the page.
            if possible_parent in components:
                parent_refdes = possible_parent.refdes_number(components)

                # or is it currently searching for a parent itself
                if '?' not in parent_refdes:
                    return parent_refdes

        return None

    for possible_parent in components:
        # is this a possible parent?
        if possible_parent.device == child.device \
               and possible_parent.slot != child.slot:

            parent_refdes = possible_parent.refdes_number(components)

            # Don't take a component as a parent which is currently
            # searching for a parent itself or doesn't have its slot
            # designated.
            if '?' not in parent_refdes and possible_parent.slot != '':
                # is my slot in this refdes already taken by another component?

                slot_taken = False
                for possible_compeditor in components:
                    if possible_compeditor.slot == child.slot \
                       and possible_compeditor.refdes_number(
                           components) == parent_refdes:
                        slot_taken = True
                        break

                # check any manual group buddies this other part might have.
                for possible_compeditor in possible_parent.group_buddies:
                    if possible_compeditor.slot == child.slot:
                        slot_taken = True
                        break

                if not slot_taken:
                    return parent_refdes

    return None


def usage():
    sys.stdout.write(_(
        "Usage:  refdes_renum_slots.py [OPTION...] FILE...\n"
"        refdes_renum_slots.py --help\n"
"        refdes_renum_slots.py --version\n"))
    sys.stdout.write("\n")
    sys.stdout.write(_(
"Renumber components in a single or multi-sheet gEDA schematic.\n"))
    sys.stdout.write("\n")
    sys.stdout.write(_(
"This program reads a gschem schematic file or files and renumbers all\n"
"referece designators.  It tries to pair up symbols with compatible slot\n"
"numbers under the same refdes number.  Groupings are determined by\n"
"proximity of the symbols to each other.\n"))
    sys.stdout.write("\n")
    sys.stdout.write(_(
"      --pgskip=N    number components on the first schematic sheet starting\n"
"                    with N+1, on the second sheet starting with 2N+1, etc.\n"
"                    (e.g., --pgskip=100 will start with 101, 201, etc.)\n"
"\n"
"  -f, --force       renumber all refdeses, even those which are already\n"
"                    numbered, but components with a common refdes will have\n"
"                    a common refdes afterwards (although maybe another one)\n"
"\n"
"  -F, --regroup     renumber all refdeses, even those which are already\n"
"                    numbered, and discard all existing manual grouping\n"
"\n"
"      --no-copy     leave the modified files in new files whose names are\n"
"                    generated by appending \".renum\" to the original file\n"
"                    names (default: overwrite the original file)\n"
"\n"
"  -v, --verbose     can talk more yes?\n"
"\n"
"      --preserve-text        don't update comments with refdes changes\n"
"\n"
"      --update-prop-refdes   update refdeses found in properties along with\n"
"                             text fields\n"
"\n"
"      --ignore-duplicate     downgrade duplicate refdes error into warning\n"
"\n"
"      --help        display this help message\n"
"      --version     show the version of this program\n"))

def version():
    sys.stdout.write(_("refdes_renum_slots version 1.5b (Pythonized)\n"))
    sys.stdout.write(_("Copyright (C) 2010 Joshua E. Lansford\n"))
    sys.stdout.write(_("Copyright (C) 2015 Roland Lutz\n"))
    sys.stdout.write("\n")
    sys.stdout.write(_(
"This program is free software; you can redistribute it and/or\n"
"modify it under the terms of the GNU General Public License\n"
"as published by the Free Software Foundation; either version 2\n"
"of the License, or (at your option) any later version.\n"))
    sys.stdout.write("\n")
    sys.stdout.write(_(
"This program is distributed in the hope that it will be useful,\n"
"but WITHOUT ANY WARRANTY; without even the implied warranty of\n"
"MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n"
"GNU General Public License for more details.\n"))

def main():
    global page_skip

    # gathered information (keys are pairs (filenumber, line_number))
    components = {}
    texts = {}

    # command line options
    force_renum = False
    no_copy = False
    regroup_flag = False
    verbose = False
    ignore_duplicate = False
    preserve_text = False
    update_prop_refdes = False

    program_short_name = os.path.basename(sys.argv[0])
    try:
        options, args = getopt.getopt(sys.argv[1:], 'fFv', [
            'pgskip=', 'force', 'regroup', 'no-copy', 'verbose',
            'preserve-text', 'update-prop-refdes', 'ignore-duplicate',
            'help', 'version'])
    except getopt.GetoptError as e:
        sys.stderr.write(_("%s: %s\n") % (program_short_name, e.msg))
        sys.stderr.write(_("Try `%s --help' for more information.\n")
                         % sys.argv[0])
        sys.exit(1)

    for option, value in options:
        if option == '--pgskip':
            # Unfortunately, Python does not support optional arguments. :(
            try:
                page_skip = int(value)
            except ValueError:
                sys.stderr.write(_("%s: Argument for --pgskip must be an "
                                   "integer\n") % program_short_name)
                sys.exit(1)
        elif option == '-f' or option == '--force':
            force_renum = True
        elif option == '-F' or option == '--regroup':
            force_renum = True
            regroup_flag = True
        elif option == '--no-copy':
            no_copy = True
        elif option == '-v' or option == '--verbose':
            verbose = True
        elif option == '--preserve-text':
            preserve_text = True
        elif option == '--update-prop-refdes':
            update_prop_refdes = True
        elif option == '--ignore-duplicate':
            ignore_duplicate = True
        elif option == '--help':
            usage()
            sys.exit(0)
        elif option == '--version':
            version()
            sys.exit(0)

    if not args:
        sys.stderr.write(_("%s: No filename given\n") % program_short_name)
        sys.stderr.write(_("Try `%s --help' for more information.\n")
                         % sys.argv[0])
        sys.exit(1)


    slot = ''
    is_partition = False
    refdes = ''
    refdes_pos = ''
    device = ''
    symbol = ''
    x = 0
    y = 0

    # first read in the information we need
    for filenumber, filename in enumerate(args):
        f = open(filename)

        text_lines_left = 0

        for line_number, line in enumerate(f):
            if text_lines_left > 0:
                if '=' in line:
                    parts = line.rstrip('\n').split('=')
                    if parts[0] == 'slot':
                        slot = parts[1]
                    elif parts[0] == 'is_partition':
                        is_partition =  parts[1].lower().startswith('t') \
                                            or parts[1].startswith('1')
                    elif parts[0] == 'refdes':
                        refdes = parts[1]
                        refdes_pos = filenumber, line_number
                    elif parts[0] == 'device':
                        device = parts[1]
                    elif update_prop_refdes:
                        texts[filenumber, line_number] = line
                else:
                    texts[filenumber, line_number] = line
                text_lines_left -= 1
            elif line.startswith('C '):
                parts = line.rstrip('\n').split(' ')
                x = int(parts[1])
                y = int(parts[2])
                symbol = parts[6]
            elif line.startswith('T '):
                parts = line.rstrip('\n').split(' ')
                text_lines_left = int(parts[9])
            elif line == '{\n':
                slot = ''
                is_partition = False
                refdes = ''
                refdes_pos = None
                device = ''
            elif line == '}\n':
                if refdes != '':
                    components[refdes_pos] = Component(
                        device, refdes, filenumber, filename,
                        slot, is_partition, x, y, symbol)

        f.close()

    # Make sure that all slotted components have a declared device type
    for component in components.values():
        if component.slot != '' and component.device == '':
            sys.stderr.write(
                _("WARNING: Symbol %s with refdes %s in %s at location " \
                  "(%d,%d) has a slot but no device type.\n") % (
                      component.symbol, component.refdes,
                      component.filename, component.x, component.y))

    # Throw an error if there is a component with a duplicate refdes
    # which isn't a partition or a slot.
    if not regroup_flag:
        for component1 in components.values():
            if '?' in component1.refdes:
                continue
            for component2 in components.values():
                if component1 == component2 \
                       or component1.refdes != component2.refdes:
                    continue
                if component1.is_partition or component1.slot != '':
                    continue
                msg = _(
"Symbol %s with refdes %s in %s at location (%d,%d) has a duplicate refdes "
"as a part at %s location (%d,%d).  It either needs a slot attribute or a "
"is_partition=true attribute set in it.") \
                    % (component1.symbol, component1.refdes,
                       component1.filename, component1.x, component1.y,
                       component2.filename, component2.x, component2.y)
                if ignore_duplicate:
                    sys.stderr.write(_("WARNING: %s\n") % msg)
                else:
                    sys.stderr.write(
                        _("ERROR: %s\n"
                          "To suppress this error, call the program "
                          "with the flag --ignore-duplicate\n") % msg)
                    sys.exit(1)

    if force_renum:
        # strip off the current numbers if --force
        # if not regrouping we need to preserve existing groups by
        # noteing which symbols share refdeses
        if not regroup_flag:
            for component1 in components.values():
                if '?' in component1.refdes:
                    continue
                for component2 in components.values():
                    if component1 != component2 \
                           and component1.refdes == component2.refdes:
                        component1.group_buddies.append(component2)

        for component in components.values():
            component.clear_refdes_number()
    else:
        # otherwise set the available page numbers to take into
        # consideration the used numbers
        for component in components.values():
            refdes_count_take_note_of(component)

    # process components in order of proximity to eachother and device type
    unprocessed_components = []
    processed_components = []

    for component in components.values():
        if '?' in component.refdes:
            unprocessed_components.append(component)
        else:
            processed_components.append(component)

    # now process the rest untell there is non left.
    while unprocessed_components:
        best_from_index = None
        best_to_index = 0
        best_distance = None

        for from_index, from_component in enumerate(unprocessed_components):
            if obtain_parent_refdes(from_component,
                                    processed_components) is not None:
                for to_index, to_component in enumerate(processed_components):
                    if from_component.device == to_component.device \
                           and (best_distance is None
                                    or from_component.distance_to(to_component)
                                            < best_distance):
                        best_from_index = from_index
                        best_to_index = to_index
                        best_distance = from_component.distance_to(to_component)

        # if we don't have a matching component to get close to then
        # we will find the component closes to the top of the page so
        # that components are numbered in order
        if best_from_index is None:
            best_from_file = None
            best_distance = None
            for from_index, from_component in enumerate(unprocessed_components):
                if best_from_file is None \
                      or from_component.filenumber < best_from_file \
                      or (from_component.filenumber == best_from_file \
                        and from_component.get_sort_height() > best_distance):
                    best_distance = from_component.get_sort_height()
                    best_from_index = from_index
                    best_from_file = from_component.filenumber
            if verbose:
                sys.stdout.write(_("New Group:\n"))

        component = unprocessed_components.pop(best_from_index)
        component.refdes_number(processed_components)
        if verbose:
            sys.stdout.write(_("\t_nameing %s %s\n") % (component.refdes,
                                                        component.device))
        processed_components.insert(best_to_index, component)


    # now update the texts with the changes of the refdeses
    if not preserve_text:
        # Find a tag that we can use to mark refdes that have already
        # been updated so we don't accidentally update a refdes tag
        # more then once.  This will make sure that update_tag is not
        # found in the text anywhere.
        update_tag = 'Updated'
        update_post_fix = 0
        found_usable_update_tag = False
        while not found_usable_update_tag:
            found_usable_update_tag = True
            for text in texts.values():
                if update_tag in text:
                    found_usable_update_tag = False
                    break

            if not found_usable_update_tag:
                update_post_fix += 1
                update_tag = 'Updated<%d>' % update_post_fix

        new_texts = {}

        for component in components.values():
            if not component.refdes_is_updated():
                continue

            old_refdes = component.old_refdes
            new_refdes = component.refdes

            # Search through the texts and find any references to the
            # old refdes and update it with the new.
            for location in texts:
                if location in new_texts:
                    text = new_texts[location]
                else:
                    text = texts[location]

                # We have to make special care not to replace
                # substrings of refdeses.
                found_location = text.index(old_refdes)
                while found_location != -1:
                    # test if this refdes is the entire refdes
                    # test front and end
                    if (found_location == 0 or not
                        is_refdes_char(text[found_location - 1])) and \
                       (found_location + len(old_refdes) == len(text) or not
                        is_refdes_char(text[found_location + len(old_refdes)])):
                        text = text[:found_location] + new_refdes + \
                          update_tag + text[found_location + len(old_refdes):]
                    found_location = text.index(old_refdes, found_location + 1)
                if text != texts[location]:
                    new_texts[location] = text

        # now update the text collection.
        for location in new_texts:
            new_text = new_texts[location]
            new_text = new_text.replace(update_tag, '')
            if verbose:
                sys.stdout.write(_("Updating text:\n"))
                sys.stdout.write('\t%s\n' % texts[location])
                sys.stdout.write('\t%s\n' % new_text)
            texts[location] = new_text

    # now do the transformation on the files
    for filenumber, filename in enumerate(args):
        inf = open(filename)

        temp_file_name = filename + ".renum"
        while os.path.exists(temp_file_name):
            temp_file_name += '~'
        outf = open(temp_file_name, 'w')

        for line_number, line in enumerate(inf):
            l = filenumber, line_number
            if l in components:
                outf.write(_("refdes=%s\n") % components[l].refdes_number(
                               processed_components))
            elif l in texts:
                outf.write(texts[l])
            else:
                outf.write(line)

        outf.flush()
        outf.close()
        del outf

        inf.close()
        del inf

        if not no_copy:
            if os.name == 'nt':
                # atomic rename/unlink is broken on Windows
                try:
                    os.unlink(filename)
                except OSError as e:
                    sys.stderr.write(_("%s: Couldn't delete old file %s: %s\n")
                                     % (program_short_name, filename,
                                        e.strerror))
                    sys.exit(1)

            try:
                os.rename(temp_file_name, filename)
            except OSError as e:
                sys.stderr.write(_("%s: Couldn't rename temp file %s "
                                   "to original name %s: %s\n")
                                 % (program_short_name, temp_file_name,
                                    filename, e.strerror))
                sys.exit(1)

    # print out some information about the components found
    components_by_device = {}
    for component in components.values():
        if component.device not in components_by_device:
            components_by_device[component.device] = []
        components_by_device[component.device].append(component)

    if verbose:
        for device_type in components_by_device:
            sys.stdout.write(_("%d\tdevices of type \"%s\"\t") %
                             (len(components_by_device[device_type]),
                              device_type))
            for component in components_by_device[device_type]:
                sys.stdout.write("%s:%d " % (component.refdes,
                                             component.filenumber + 1))
            sys.stdout.write("\n")

    # Print a warning for groups which contain components of seperate
    # device type or the same slot number.
    for component1 in components.values():
        for component2 in components.values():
            if component1 == component2 \
                   or component1.refdes != component2.refdes:
                continue
            if component1.device != component2.device:
                mgs = _("of different device types")
            elif component1.slot and component1.slot == component2.slot:
                msg = _("with same slot number")
            else:
                continue
            sys.stderr.write(_(
"WARNING: Same refdes given to components %s:\n"
"\t1) %s with refdes %s and device %s in %s at location (%d,%d)\n"
"\t2) %s with refdes %s and device %s in %s at location (%d,%d)\n")
              % (msg, component1.symbol, component1.refdes, component1.device,
                      component1.filename, component1.x, component1.y,
                      component2.symbol, component2.refdes, component2.device,
                      component2.filename, component2.x, component2.y))

if __name__ == '__main__':
    main()
