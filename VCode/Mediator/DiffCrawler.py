##############################################################################
# VoiceCode, a programming-by-voice environment
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
#
# (C)2003, David C. Fox
#
##############################################################################

"""system for comparing hierarchical Python objects by introspection
"""

from Object import Object
import types
import string

class DiffReport(Object):
    """abstract base class for reporting differences between two
    hierarchical objects or variables

    **INSTANCE ATTRIBUTES**

    *[ANY] element_names* -- 
    the list of names or indices giving the path from the element which 
    differs, up the element/attribute hierarchy to the original
    variables being compared.  The names may be of any immutable type,
    but are usually a STR or an INT, e.g. the key for an element 
    of a dictionary, the index for the element of a list, the 
    attribute name for an attribute of an object.  The list starts with
    the location of the difference and works its way up the hierarchy.

    *[STR]* element_types -- name of the type of the corresponding 
    elements: 'item' for a dictionary value, 'element' for a list element, 
    'attribute' for an object attribute.
    """
    def __init__(self, init_path = None, **args):
        """
        Initialize the report with the description of the bottom level
        of the hierarchy where the difference was found

        **INPUTS**

        *[(ANY, STR)] init_path -- list of (element_name, element_type) tuples
        indicating the path to the difference, starting with the
        location and working up the hierarchy, or None to add links
        later
        """
        self.deep_construct(DiffReport, 
                            {'element_names': [],
                             'element_types': []},
                            args)
        if init_path:
            for (el_name, el_type) in init_path:
                self.add_link(el_name, el_type)

    def add_link(self, el_name, el_type):
        """add another link to the path from the element which differs
        up to the top of the hierarchy

        **INPUTS**

        *ANY immutable (but usually STR or INT) el_name* -- 
        the name of the next element in the path.  see class docstring above

        *STR* el_type -- the name of the type of that element: 'item'
        for a dictionary value, 'element' for a list element, 
        'attribute' for an object attribute.

        **OUTPUTS**

        *none*
        """
        self.element_names.append(el_name)
        self.element_types.append(el_type)

    def insert_link(self, el_name, el_type):
        """prepend another link to the path from the element which differs
        up to the top of the hierarchy

        **INPUTS**

        *ANY immutable (but usually STR or INT) el_name* -- 
        the name of the first element in the path.  see class docstring above

        *STR* el_type -- the name of the type of that element: 'item'
        for a dictionary value, 'element' for a list element, 
        'attribute' for an object attribute.

        **OUTPUTS**

        *none*
        """
        self.element_names.insert(0, el_name)
        self.element_types.insert(0, el_type)

    def path(self):
        """The raw path information to the location of the difference

        **INPUTS**

        *none*

        **OUTPUTS**

        [(ANY, STR)] -- list of (element_name, element_type) tuples
        indicating the path to the difference, starting with the
        location and working up the hierarchy.
        """
        return zip(self.element_names, self.element_types)

    def location(self):
        """A string describing the path to the location of the difference

        **INPUTS**

        *none*

        **OUTPUTS**

        *STR* -- the path description (e.g. "element 0 of attribute
        'symbols')
        """
        out = []
        for name, el_type in self.path():
            out.append("%s %s" % (el_type, repr(name)))
        return string.join(out, ' of ')

    def description(self):
        """a description of the difference

        **INPUTS**

        *none*

        **OUTPUTS**

        *STR* -- a string describing the actual difference
        """
        debug.virtual('DiffReport.description')

class MissingElement(DiffReport):
    """concrete subclass of DiffReport indicating that one object is
    missing a given element (list element, key/value pair, or attribute)
    """
    def __init__(self, first, **args):
        """
        **INPUTS**

        *BOOL first* -- 1 if the element is missing from the first object 
        being compared, or 0 if it is missing from the second object
        """
        self.deep_construct(MissingElement, 
                            {'first': first},
                            args)

    def description(self):
        """a description of the difference

        **INPUTS**

        *none*

        **OUTPUTS**

        *STR* -- a string describing the actual difference
        """
        s = "second"
        if self.first:
            s = "first"
        return "missing from %s argument" % s

class TypeMismatch(DiffReport):
    """concrete subclass of DiffReport indicating that the corresponding
    elements of the two objects have different types or classes.
    """
    def __init__(self, first, second, **args):
        """
        **INPUTS**

        *TypeType first, second* -- the types of the corresponding
        elements.
        """
        self.deep_construct(TypeMismatch, 
                            {'first': repr(first),
                             'second': repr(second)},
                            args)

    def description(self):
        """a description of the difference

        **INPUTS**

        *none*

        **OUTPUTS**

        *STR* -- a string describing the actual difference
        """
        return "%s doesn't match %s" % (self.first, self.second)

class EmptyContainer(DiffReport):
    """concrete subclass of DiffReport indicating that a container from
    one of the two objects is empty, making it impossible to compare
    the type of the elements of the container.  This report should only
    be generated by struct_diff, since a complete difference would
    simply indicate elements of the container which were missing from
    one object (or would compare equal if both objects had empty
    containers)
    """
    def __init__(self, which, container, **args):
        """
        **INPUTS**

        *INT which* -- two bit flag indicating which object has the
        empty container (1 = first, 2 = second, 3 = both)

        *STR container* -- the type of container
        """
        self.deep_construct(EmptyContainer, 
                            {'which': which, 
                             'container': container},
                            args)

    def description(self):
        """a description of the difference

        **INPUTS**

        *none*

        **OUTPUTS**

        *STR* -- a string describing the actual difference
        """
        if self.which == 1:
            s = "first object has"
        elif self.which == 2:
            s = "second object has"
        else:
            s = "both objects have"
        return "%s an empty %s" % (s, self.container)

class HeterogeneousContainer(DiffReport):
    """concrete subclass of DiffReport indicating that container from
    one or both of the objects contain heterogeneous elements,
    making it impossible to compare their types.  This report should only
    be generated by struct_diff, since a complete difference would
    simply compare elements one by one
    """
    def __init__(self, first, container, el_type, **args):
        """
        **INPUTS**

        *BOOL first* -- 1 if the heterogeneous container is from the 
        first object being compared, or 0 if it is from the second object

        *STR container* -- the type of container

        *STR el_type* -- element type ('keys', 'values', 'elements' 
        (for lists or tuples))
        """
        self.deep_construct(HeterogeneousContainer, 
                            {'first': first,
                            'container': container,
                             'el_type': el_type},
                            args)

    def description(self):
        """a description of the difference

        **INPUTS**

        *none*

        **OUTPUTS**

        *STR* -- a string describing the actual difference
        """
        s = "second"
        if self.first:
            s = "first"
        description = "%s variable has a %s" % (s, self.container) + \
            " with heterogeneous %s,\n" % self.el_type + \
            "so the structure of elements of the container can't be compared"
        return description

class ValueDiff(DiffReport):
    """concrete subclass of DiffReport indicating that the corresponding
    elements are built-in types or C extension classes with different
    values.
    """
    def __init__(self, first, second, **args):
        """
        **INPUTS**

        *ANY first, second* -- the corresponding elements.
        """
        self.deep_construct(TypeMismatch, 
                            {'first': repr(first),
                             'second': repr(second)},
                            args)

    def description(self):
        """a description of the difference

        **INPUTS**

        *none*

        **OUTPUTS**

        *STR* -- a string describing the actual difference
        """
        return "value %s doesn't match %s" % (self.first, self.second)

def type_or_class(x):
    """returns the type of a variable x, or its class when the latter is
    more specific, so that the types of two variables can be compared
    """
    t = type(x)
    if t is types.InstanceType:
# for old-style classes, __class__ is more specific
        return x.__class__
# for built-in types and new-style classes
    return t

def homogeneous_types(s):
    """checks whether the types of elements of a sequence s are
    homogeneous

    **INPUTS***

    *SEQ s* -- a sequence whose elements are expected to be of
    homogeneous type.  Note: this function requires EXACTLY the same
    types.  Instances with a common superclass do not match (because
    in Python 2.2 or greater they could all subclasses of object, and 
    that wouldn't mean much)

    **OUTPUTS**

    *BOOL* -- true if the types of elements of s are homogenous
    """
    expected = type_or_class(s[0])
    for element in s[1:]:
        if type_or_class(element) != expected:
            return 0
    return 1

class DiffCrawler(Object):
    """abstract base class for comparing two objects.  Each crawler
    should be used only once.  See the subclasses for information on
    retrieving the difference reports.  Normally, the user calls only
    the compare method
    """ 
    def __init__(self, **args):
        self.deep_construct(DiffCrawler, {}, args)

    def report_diff(self, difference):
        """report a difference between the two objects being compared

        **INPUTS**

        *DiffReport difference* -- the report describing the difference
        between corresponding elements of the objects being compared

        **OUTPUTS**

        *STR* -- 'prune' to skip comparing children of the element being
        compared, 'finish' to skip the rest of the elements entirely, or
        None to continue comparing children of the elements in question
        """
# by making this virtual, we allow for subclasses to return a list of
# all differences, or just the first difference encountered
        debug.virtual('DiffReport.report_diff')

    def compare(self, x, y, path = None):
        """compare elements of two objects, storing the results
        internally (see the concrete subclass for the format of the
        results and how to retrieve them).

        **INPUTS**

        *ANY x, y* -- objects being compared, or their components (when
        called internally)

        *[(ANY, STR)] path* -- list of (element_name, element_type) tuples
        indicating the path to the elements being compared, starting with the
        location and working up the hierarchy to the original objects
        being compared, or None if x and y are the original objects.

        **OUTPUTS**

        *STR* -- 'finish' to skip comparing the siblings or uncles/aunts 
        of these elements, if any, or None to continue comparing them
        """
        if path is None:
            path = []
        disposition = self._compare_types(x, y, path)
        if disposition:
            return disposition
        if isinstance(x, type({})):
            disposition = self._compare_dictionaries(x, y, path)
            if disposition:
                return disposition
            disposition = self._compare_items(x, y, path)
            if disposition:
                return disposition
        elif isinstance(x, type([])):
            disposition = self._compare_lists(x, y, path)
            if disposition:
                return disposition
            disposition = self._compare_elements(x, y, path)
            if disposition:
                return disposition
        elif isinstance(x, type(())):
            disposition = self._compare_tuples(x, y, path)
            if disposition:
                return disposition
            disposition = self._compare_elements(x, y, path)
            if disposition:
                return disposition
        elif not hasattr(x, '__dict__'):
            disposition = self._compare_values(x, y, path)
            return disposition
        if hasattr(x, '__dict__'):
            disposition = self._compare_objects(x, y, path)
            if disposition:
                return disposition
            disposition = self._compare_items(x.__dict__, y.__dict__, 
                path, objects = 1)
            if disposition:
                return disposition
            
    def _compare_items(self, x, y, path, objects = 0):
        """Private method to compare items in dictionaries

        *NOTE:* This method is called internally by compare,
        and should not normally be called externally.

        **INPUTS**

        *{ANY: ANY} x, y* -- components of the objects being compared,
        which are dictionaries (or subclasses of dict), or which
        implement the appropriate methods (keys, __getitem__(key),
        items)

        *[(ANY, STR)] path* -- list of (element_name, element_type) tuples
        indicating the path to the elements being compared, starting with the
        location and working up the hierarchy to the original objects
        being compared, or None if x and y are the original objects.

        *BOOL objects* -- if true, then the dictionaries are the
        __dict__ attributes of objects, so their items are called
        attributes rather than items.

        **OUTPUTS**

        *STR* -- 'prune' to skip comparing children of the element being
        compared, 'finish' to skip the rest of the elements entirely, or
        None to continue comparing children of the elements in question
        """
        first_keys = x.keys()
        first_keys.sort()
        second_keys = y.keys()
        second_keys.sort()
        common_keys = []
        el_name = 'item'
        if objects:
            el_name = 'attribute'
        for key in first_keys:
            if y.has_key(key):
                common_keys.append(key)
            else:
                d = MissingElement(first = 0, init_path = path)
                d.insert_link(key, el_name)
                disposition = self.report_diff(d)
                if disposition == 'finish':
                    return disposition
        for key in second_keys:
            if not x.has_key(key):
                d = MissingElement(first = 1, init_path = path)
                d.insert_link(key, el_name)
                disposition = self.report_diff(d)
                if disposition == 'finish':
                    return disposition
        for key in common_keys:
            item_path = [(key, el_name)]
            item_path.extend(path)
            disposition = self.compare(x[key], y[key], item_path)
            if disposition == 'finish':
                return disposition
        return None

    def _compare_types(self, x, y, path):
        """Private method to compare types of elements of two objects, 
        storing the results internally (see the concrete subclass for 
        the format of the results and how to retrieve them).

        *NOTE:* This method is called internally by compare,
        and should not normally be called externally.

        **INPUTS**

        *ANY x, y* -- components of the objects being compared

        *[(ANY, STR)] path* -- list of (element_name, element_type) tuples
        indicating the path to the elements being compared, starting with the
        location and working up the hierarchy to the original objects
        being compared, or None if x and y are the original objects.

        **OUTPUTS**

        *STR* -- 'prune' to skip comparing children of the element being
        compared, 'finish' to skip the rest of the elements entirely, or
        None to continue comparing children of the elements in question
        """
        if path is None:
            path = []
        type_x = type_or_class(x)
        type_y = type_or_class(y)
        if type_x == type_y:
            return None
        d = TypeMismatch(type_x, type_y, init_path = path)
        return self.report_diff(d)

    def _compare_elements(self, x, y, path):
        """Private method to compare elements in sequences

        *NOTE:* This method is called internally by compare,
        and should not normally be called externally.

        **INPUTS**

        *[ANY] x, y* -- components of the objects being compared,
        which are sequences to be compared item by item

        *[(ANY, STR)] path* -- list of (element_name, element_type) tuples
        indicating the path to the elements being compared, starting with the
        location and working up the hierarchy to the original objects
        being compared, or None if x and y are the original objects.

        **OUTPUTS**

        *STR* -- 'prune' to skip comparing children of the sequence being
        compared, 'finish' to skip the rest of the elements entirely, or
        None to continue comparing children of the sequence in question
        """
        xl = len(x)
        yl = len(y)
        shorter = min(xl, yl)
        longer = max(xl, yl)
        for i in range(shorter):
            el_path = [(i, 'element')]
            el_path.extend(path)
            disposition = self.compare(x[i], y[i], el_path)
            if disposition == 'finish':
                return disposition
        if longer > shorter:
            first = 0
            if yl > xl:
                first = 1
            for i in range(shorter, longer):
                d = MissingElement(first, init_path = path)
                d.insert_link(i, 'element')
                disposition = self.report_diff(d)
                if disposition == 'finish':
                    return disposition
        return None

    def _compare_objects(self, x, y, path):
        """Private method to compare arbitrary objects.
        If the crawler requires special handling of 
        particular types of objects, it should override this method 
        and compare them itself, calling report_diff, and should 
        return the appropriate
        instructions.

        *NOTE:* This method is called internally by compare,
        and should not normally be called externally.

        **INPUTS**

        *{ANY: ANY} x, y* -- components of the objects being compared

        *[(ANY, STR)] path* -- list of (element_name, element_type) tuples
        indicating the path to the elements being compared, starting with the
        location and working up the hierarchy to the original objects
        being compared, or None if x and y are the original objects.

        **OUTPUTS**

        *STR* -- 'prune' to skip comparing children of the element being
        compared, 'finish' to skip the rest of the elements entirely, or
        None to continue comparing children of the elements in question
        """
# default implementation - compare method will proceed to compare the
# objects attribute by attribute
        return None


    def _compare_dictionaries(self, x, y, path):
        """Private method to compare dictionaries.  If the particular
        DiffCrawler subclass expects items in the two dictionaries to 
        correspond in detail, it should simply return None, and the 
        compare method will proceed to compare them item by item.  
        If the particular crawler requires special handling of 
        dictionaries, it should override this method and compare them
        itself, calling report_diff, and should return the appropriate
        instructions.

        *NOTE:* This method is called internally by compare,
        and should not normally be called externally.

        **INPUTS**

        *{ANY: ANY} x, y* -- components of the objects being compared,
        which are dictionaries (or subclasses of dict)

        *[(ANY, STR)] path* -- list of (element_name, element_type) tuples
        indicating the path to the elements being compared, starting with the
        location and working up the hierarchy to the original objects
        being compared, or None if x and y are the original objects.

        **OUTPUTS**

        *STR* -- 'prune' to skip comparing children of the element being
        compared, 'finish' to skip the rest of the elements entirely, or
        None to continue comparing children of the elements in question
        """
# default implementation - compare method will proceed to compare the
# dictionaries item by item
        return None

    def _compare_lists(self, x, y, path):
        """Private method to compare lists.  If the particular
        DiffCrawler subclass expects elements of the two lists to 
        correspond in detail, it should simply return None, and the 
        compare method will proceed to compare them element by element.
        If the particular crawler requires special handling of 
        lists, it should override this method and compare them
        itself, calling report_diff, and should return the appropriate
        instructions.

        *NOTE:* This method is called internally by compare,
        and should not normally be called externally.

        **INPUTS**

        *[ANY] x, y* -- components of the objects being compared,
        which are lists (or subclasses of list)

        *[(ANY, STR)] path* -- list of (element_name, element_type) tuples
        indicating the path to the elements being compared, starting with the
        location and working up the hierarchy to the original objects
        being compared, or None if x and y are the original objects.

        **OUTPUTS**

        *STR* -- 'prune' to skip comparing children of the element being
        compared, 'finish' to skip the rest of the elements entirely, or
        None to continue comparing children of the elements in question
        """
# default implementation - compare method will proceed to compare the
# dictionaries item by item
        return None

    def _compare_tuples(self, x, y, path):
        """Private method to compare tuples.  If the particular
        DiffCrawler subclass expects elements of the two tuples to 
        correspond in detail, it should simply return None, and the 
        compare method will proceed to compare them element by element.
        If the particular crawler requires special handling of 
        tuples, it should override this method and compare them
        itself, calling report_diff, and should return the appropriate
        instructions.

        *NOTE:* This method is called internally by compare,
        and should not normally be called externally.

        **INPUTS**

        *(ANY) x, y* -- components of the objects being compared,
        which are tuples (or subclasses of tuple)

        *[(ANY, STR)] path* -- list of (element_name, element_type) tuples
        indicating the path to the elements being compared, starting with the
        location and working up the hierarchy to the original objects
        being compared, or None if x and y are the original objects.

        **OUTPUTS**

        *STR* -- 'prune' to skip comparing children of the element being
        compared, 'finish' to skip the rest of the elements entirely, or
        None to continue comparing children of the elements in question
        """
# default implementation - compare method will proceed to compare the
# dictionaries item by item
        return None

    def _compare_values(self, x, y, path):
        """Private method to compare values of built-in or extension
        types.  The default is to use == to compare the two. 
        If the particular crawler requires special handling (e.g. if it
        only wants to compare types), it should override this method and 
        compare them itself, calling report_diff, and should return 
        the appropriate instructions.

        *NOTE:* This method is called internally by compare,
        and should not normally be called externally.

        **INPUTS**

        *ANY x, y* -- components of the objects being compared,
        which are built-in or extension types

        *[(ANY, STR)] path* -- list of (element_name, element_type) tuples
        indicating the path to the elements being compared, starting with the
        location and working up the hierarchy to the original objects
        being compared, or None if x and y are the original objects.

        **OUTPUTS**

        *STR* -- 'prune' to skip comparing children of the element being
        compared, 'finish' to skip the rest of the elements entirely, or
        None to continue comparing children of the elements in question
        """
# default implementation 
        if x == y:
            return None
        d = ValueDiff(x, y, init_path = path)
        return self.report_diff(d)

class ObjDiff(DiffCrawler):
    """concrete subclass of DiffCrawler with default behavior (a detailed,
    hierarchical comparison of two objects)
    """
    def __init__(self, all = 0, **args):
        """
        **INPUTS**

        *BOOL all* -- if true, report all differences, otherwise
        return after finding the first difference
        """
        self.deep_construct(ObjDiff, {'all': all, 'reports': []}, args)

    def clear(self):
        """clear the list of differences
        """
        self.reports = []

    def differences(self):
        """returns the differences found

        **OUTPUTS**

        *[DiffReport]* -- list of differences, or None if none were
        found.  Unless, all = 1 was specified when the ObjDiff object
        was created, this list will have only one element.
        """
        if self.reports:
            return self.reports
        return None

    def report_diff(self, difference):
        """report a difference between the two objects being compared

        **INPUTS**

        *DiffReport difference* -- the report describing the difference
        between corresponding elements of the objects being compared

        **OUTPUTS**

        *STR* -- 'prune' to skip comparing children of the element being
        compared, 'finish' to skip the rest of the elements entirely, or
        None to continue comparing children of the elements in question
        """
# by making this virtual, we allow for subclasses to return a list of
# all differences, or just the first difference encountered
        self.reports.append(difference)
        if self.all:
            return 'prune'
        else:
            return 'finish'

class StructDiff(ObjDiff):
    """concrete subclass of DiffCrawler which compares the structure 
    of two variables.  All object attributes are compared attribute by
    attribute, but two values match if they have the same type, and two
    containers match if their elements have the same types.  Optionally,
    StructDiff can also require that containers be non-empty and that
    their elements have homogeneous types (otherwise it can't make a 
    meaningful type comparison)
    """
    def __init__(self, ignore_empty = 0, ignore_heterogeneous = 0, 
        **args):
        """
        **INPUTS**

        *BOOL ignore_empty* -- if true, do not report empty lists or
        dictionaries (whose elements therefore cannot be compared
        structurally) as a difference

        *BOOL ignore_heterogeneous* -- if true, do not report lists with
        heterogeneous types or dictionaries with heterogeneously typed keys
        or values as a difference
        """
        self.deep_construct(StructDiff, 
                            {
                             'ignore_empty': ignore_empty,
                             'ignore_heterogeneous': ignore_heterogeneous
                            }, args)

    def _compare_objects(self, x, y, path):
        """Private method to compare arbitrary objects.
        If the crawler requires special handling of 
        particular types of objects, it should override this method 
        and compare them itself, calling report_diff, and should 
        return the appropriate
        instructions.

        *NOTE:* This method is called internally by compare,
        and should not normally be called externally.

        **INPUTS**

        *{ANY: ANY} x, y* -- components of the objects being compared

        *[(ANY, STR)] path* -- list of (element_name, element_type) tuples
        indicating the path to the elements being compared, starting with the
        location and working up the hierarchy to the original objects
        being compared, or None if x and y are the original objects.

        **OUTPUTS**

        *STR* -- 'prune' to skip comparing children of the element being
        compared, 'finish' to skip the rest of the elements entirely, or
        None to continue comparing children of the elements in question
        """
        if not hasattr(x, 'items'):
            return None
        first_items = x.items()
        second_items = y.items()
        which_empty = 0
        if not first_items:
            which_empty = which_empty | 1
        if not second_items:
            which_empty = which_empty | 2
        if which_empty:
            if self.ignore_empty:
                return None
            d = EmptyContainer(which_empty, container = 'mapping', 
                init_path = path)
            return self.report_diff(d)
        first_keys, first_values = apply(zip, first_items)
        if not homogeneous_types(first_keys):
            if self.ignore_heterogeneous:
                return None
            d = HeterogeneousContainer(first = 1, container = 'mapping', 
                el_type = 'keys', init_path = path)
            return self.report_diff(d)
        if not homogeneous_types(first_values):
            if self.ignore_heterogeneous:
                return None
            d = HeterogeneousContainer(first = 1, container = 'mapping', 
                el_type = 'values', init_path = path)
            return self.report_diff(d)
        second_keys, second_values = apply(zip, second_items)
        if not homogeneous_types(second_keys):
            if self.ignore_heterogeneous:
                return None
            d = HeterogeneousContainer(first = 0, container = 'mapping', 
                el_type = 'keys', init_path = path)
            return self.report_diff(d)
        if not homogeneous_types(second_values):
            if self.ignore_heterogeneous:
                return None
            d = HeterogeneousContainer(first = 0, container = 'mapping', 
                el_type = 'values', init_path = path)
            return self.report_diff(d)
        key_path = [((first_keys[0], second_keys[0]), 'key')]
        key_path.extend(path)
        disposition = self._compare_types(first_keys[0], second_keys[0], key_path)
        if disposition == 'finish':
            return disposition
        value_path = [((first_keys[0], second_keys[0]), 'value[key]')]
        value_path.extend(path)
#        disposition = self._compare_types(x.values()[0], y.values()[0],
#            value_path)
        disposition = self.compare(first_values[0], second_values[0],
            value_path)
        if disposition:
            return disposition
        return "prune"

    def _compare_dictionaries(self, x, y, path):
        """Private method to compare dictionaries.  If the particular
        DiffCrawler subclass expects items in the two dictionaries to 
        correspond in detail, it should simply return None, and the 
        compare method will proceed to compare them item by item.  
        If the particular crawler requires special handling of 
        dictionaries, it should override this method and compare them
        itself, calling report_diff, and should return the appropriate
        instructions.

        *NOTE:* This method is called internally by compare,
        and should not normally be called externally.

        **INPUTS**

        *{ANY: ANY} x, y* -- components of the objects being compared,
        which are dictionaries (or subclasses of dict)

        *[(ANY, STR)] path* -- list of (element_name, element_type) tuples
        indicating the path to the elements being compared, starting with the
        location and working up the hierarchy to the original objects
        being compared, or None if x and y are the original objects.

        **OUTPUTS**

        *STR* -- 'prune' to skip comparing children of the element being
        compared, 'finish' to skip the rest of the elements entirely, or
        None to continue comparing children of the elements in question
        """
        first_keys = x.keys()
        first_keys.sort()
        second_keys = y.keys()
        second_keys.sort()
        which_empty = 0
        if not first_keys:
            which_empty = which_empty | 1
        if not second_keys:
            which_empty = which_empty | 2
        if which_empty:
            if self.ignore_empty:
                return None
            d = EmptyContainer(which_empty, container = 'dictionary', 
                init_path = path)
            return self.report_diff(d)
        if not homogeneous_types(x.keys()):
            if self.ignore_heterogeneous:
                return None
            d = HeterogeneousContainer(first = 1, container = 'dictionary', 
                el_type = 'keys', init_path = path)
            return self.report_diff(d)
        if not homogeneous_types(x.values()):
            if self.ignore_heterogeneous:
                return None
            d = HeterogeneousContainer(first = 1, container = 'dictionary', 
                el_type = 'values', init_path = path)
            return self.report_diff(d)
        if not homogeneous_types(y.keys()):
            if self.ignore_heterogeneous:
                return None
            d = HeterogeneousContainer(first = 0, container = 'dictionary', 
                el_type = 'keys', init_path = path)
            return self.report_diff(d)
        if not homogeneous_types(y.values()):
            if self.ignore_heterogeneous:
                return None
            d = HeterogeneousContainer(first = 0, container = 'dictionary', 
                el_type = 'values', init_path = path)
            return self.report_diff(d)
        key_path = [((first_keys[0], second_keys[0]), 'key')]
        key_path.extend(path)
        disposition = self._compare_types(first_keys[0], second_keys[0], key_path)
        if disposition == 'finish':
            return disposition
        value_path = [((first_keys[0], second_keys[0]), 'value[key]')]
        value_path.extend(path)
#        disposition = self._compare_types(x.values()[0], y.values()[0],
#            value_path)
        disposition = self.compare(x[first_keys[0]], y[second_keys[0]],
            value_path)
        if disposition:
            return disposition
        return "prune"

    def _compare_lists(self, x, y, path):
        """Private method to compare lists.  If the particular
        DiffCrawler subclass expects elements of the two lists to 
        correspond in detail, it should simply return None, and the 
        compare method will proceed to compare them element by element.
        If the particular crawler requires special handling of 
        lists, it should override this method and compare them
        itself, calling report_diff, and should return the appropriate
        instructions.

        *NOTE:* This method is called internally by compare,
        and should not normally be called externally.

        **INPUTS**

        *[ANY] x, y* -- components of the objects being compared,
        which are lists (or subclasses of list)

        *[(ANY, STR)] path* -- list of (element_name, element_type) tuples
        indicating the path to the elements being compared, starting with the
        location and working up the hierarchy to the original objects
        being compared, or None if x and y are the original objects.

        **OUTPUTS**

        *STR* -- 'prune' to skip comparing children of the element being
        compared, 'finish' to skip the rest of the elements entirely, or
        None to continue comparing children of the elements in question
        """
        which_empty = 0
        if not len(x):
            which_empty = which_empty | 1
        if not len(y):
            which_empty = which_empty | 2
        if which_empty:
            if self.ignore_empty:
                return None
            d = EmptyContainer(which_empty, container = 'list', 
                init_path = path)
            return self.report_diff(d)
        if not homogeneous_types(x):
            if self.ignore_heterogeneous:
                return None
            d = HeterogeneousContainer(first = 1, container = 'list', 
                el_type = 'elements', init_path = path)
            return self.report_diff(d)
        if not homogeneous_types(y):
            if self.ignore_heterogeneous:
                return None
            d = HeterogeneousContainer(first = 0, container = 'list', 
                el_type = 'elements', init_path = path)
            return self.report_diff(d)
        el_path = [(0, 'element')]
        el_path.extend(path)
#        disposition = self._compare_types(x[0], y[0], el_path)
        disposition = self.compare(x[0], y[0], el_path)
        if disposition:
            return disposition
        return "prune"

    def _compare_tuples(self, x, y, path):
        """Private method to compare tuples.  If the particular
        DiffCrawler subclass expects elements of the two tuples to 
        correspond in detail, it should simply return None, and the 
        compare method will proceed to compare them element by element.
        If the particular crawler requires special handling of 
        tuples, it should override this method and compare them
        itself, calling report_diff, and should return the appropriate
        instructions.

        *NOTE:* This method is called internally by compare,
        and should not normally be called externally.

        **INPUTS**

        *(ANY) x, y* -- components of the objects being compared,
        which are tuples (or subclasses of tuple)

        *[(ANY, STR)] path* -- list of (element_name, element_type) tuples
        indicating the path to the elements being compared, starting with the
        location and working up the hierarchy to the original objects
        being compared, or None if x and y are the original objects.

        **OUTPUTS**

        *STR* -- 'prune' to skip comparing children of the element being
        compared, 'finish' to skip the rest of the elements entirely, or
        None to continue comparing children of the elements in question
        """
        which_empty = 0
        if not len(x):
            which_empty = which_empty | 1
        if not len(y):
            which_empty = which_empty | 2
        if which_empty:
            if self.ignore_empty:
                return None
            d = EmptyContainer(which_empty, container = 'list', 
                init_path = path)
            return self.report_diff(d)
        if not homogeneous_types(x):
            if self.ignore_heterogeneous:
                return None
            d = HeterogeneousContainer(first = 1, container = 'tuple', 
                el_type = 'elements', init_path = path)
            return self.report_diff(d)
        if not homogeneous_types(y):
            if self.ignore_heterogeneous:
                return None
            d = HeterogeneousContainer(first = 0, container = 'tuple', 
                el_type = 'elements', init_path = path)
            return self.report_diff(d)
        el_path = [(0, 'element')]
        el_path.extend(path)
#        disposition = self._compare_types(x[0], y[0], el_path)
        disposition = self.compare(x[0], y[0], el_path)
        if disposition:
            return disposition
        return "prune"

    def _compare_values(self, x, y, path):
        """Private method to compare values of built-in or extension
        types.  The default is to use == to compare the two. 
        If the particular crawler requires special handling (e.g. if it
        only wants to compare types), it should override this method and 
        compare them itself, calling report_diff, and should return 
        the appropriate instructions.

        *NOTE:* This method is called internally by compare,
        and should not normally be called externally.

        **INPUTS**

        *ANY x, y* -- components of the objects being compared,
        which are built-in or extension types

        *[(ANY, STR)] path* -- list of (element_name, element_type) tuples
        indicating the path to the elements being compared, starting with the
        location and working up the hierarchy to the original objects
        being compared, or None if x and y are the original objects.

        **OUTPUTS**

        *STR* -- 'prune' to skip comparing children of the element being
        compared, 'finish' to skip the rest of the elements entirely, or
        None to continue comparing children of the elements in question
        """
# compare will already have compared types, so we just ignore the values
        return None

