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
# (C)2002, National Research Council of Canada
#
##############################################################################

"""classes for doing fancy namespace manipulations
"""
import types
import re

def make_regex(s):
    """given a string or compiled regular expression object
    (re.RegexObject), return a compiled regular expression object

    *STR or re.RegexObject s* -- regulare expression to convert, or None
    for a regular expression which matches anything
    """
    any = re.compile('')
    if type(s) == types.StringType:
        return re.compile(s)
    if type(s) == type(any):
        return s
    return any

def selected_methods(the_class, include = None,
    exclude = None):
    """returns a list of the methods of a given class matching the
    include regular expression and not matching the exclude regular
    expression.
    
    **NOTE:** this function will not return methods of any superclasses,
    only the class itself.  It must be called explicitly for each
    superclass whose methods are desired

    **INPUTS**

    *CLASS the_class* -- class object 

    *STR or re.RegexObject include* -- include only methods 
    whose names match this regex, or None to include all except those
    matching exclude.  Note: we use re.search, so if you want to match
    characters at the start of the method name, you need an explicit
    '^'.

    *STR or re.RegexObject exclude* -- exclude methods whose 
    names match this regex, or None to include all methods matching
    include.  Note: we use re.search, so if you want to match characters
    at the start of the method name, you need an explicit '^'.

    **OUTPUTS**

    [STR] -- names of the methods
    """
    compiled_in = make_regex(include)
    compiled_out = make_regex(exclude)
    methods = []
    for key, value in the_class.__dict__.items():
# for some reason, the_class.__dict__['method_name'] is of type function, 
# whereas getattr(the_class, 'method_name') is of type method
        if type(value) != types.FunctionType:
            continue
        if not compiled_in.search(key):
            continue
        if compiled_out.search(key):
            continue
        methods.append(key)
    return methods
        
class InstanceSpace:
    def bind_to_space(self, names, methods):
        """add bound copies of specified methods to the given dictionary

        **NOTE:** bind_to_space doesn't check that the named methods are
        actually methods

        **INPUTS**

        *{STR:ANY} names* -- dictionary into which to insert the  bound
        methods

        *[STR] methods* -- the list of method names to insert
        """
        for method in methods:
            names[method] = getattr(self, method)
            

