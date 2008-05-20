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
# (C)2000, National Research Council of Canada
#
##############################################################################

import exceptions, sys

class CancelTesting(RuntimeError):
    def __init__(self, msg = None):
        if msg is None:
            msg = "Cancelled regression testing"
        RuntimeError.__init__(self, msg)
#

"""Functions for debugging purposes."""

def not_implemented(name):
    """Prints warning message when a stub function is called."""
    print "WARNING: stub function %s is not implemented yet!!!" % name

def virtual(name):
    """Prints warning message when a virtual method is called."""
    print "WARNING: virtual method '%s' called!!!" % name    

def critical_warning(warn):
    """Prints a critical warning message.
    
    A critical warning usually indicates a bug in the code which should
    be reported and resolved, but in a circumstance where the best 
    immediate response is to ignore and continue running.  For example, 
    if an error is detected by an object destructor or during cleanup, it 
    may be advisable to proceed to free the rest of the resources used
    by the object."""
#   for now, just write to stderr.  Later, we may want to log this to a
#   file and/or tell the user to submit a bug report
    sys.stderr.write(warn)



def print_call_stack(print_to_file=sys.stdout):
    """Prints the call stack.

    This is done by raising an exception, catching it and printing the
    traceback object. In Python 2, there is a more direct way of doing this.
    """
    try:
        raise exceptions.Exception()
    except exceptions.Exception, err:        
        traceback.print_stack(file=print_to_file)

def trace_call_stack(trace_id, location_id=None, print_to_file=sys.stdout):
    if trace_is_active(trace_id):
       print_to_file.write("-- %s: call stack at location '%s' is:\n" % 
       (trace_id, location_id))
       print_call_stack(print_to_file)


def what_class(instance):
    """Returns a string describing the class of an instance.

    It works with any Python class or Python standard data types (int, float,
    string, etc.), but not with extension classes."""

    is_class = 'unknown'
    try:
        tmp = instance.__class__
        is_class = tmp
    except exceptions.AttributeError:
        #
        # The instance is not a python class. Maybe one of the
        # standard python data types?
        #
        is_class = type(instance)

    return is_class

def methods(instance):
    methods = {}
    inst_class = instance.__class__
    for method_name in inst_class.__dict__.keys():
        if isinstance(inst_class.__dict__[method_name], types.FunctionType):
            methods[method_name] = inst_class.__dict__[method_name]
    return methods




###############################################################################
# Managing trace printing
###############################################################################

def dont_print_trace(trace_id, message, insert_nl=1):
    """Just ignore traces. Used for greater efficiency."""
    pass

def print_trace(trace_id, message, insert_nl=1):
    global to_be_traced, trace_file

#    print '-- debug.print_trace: trace_id=%s, trace_is_active(trace_id)=%s' % (trace_id, trace_is_active(trace_id))
    if trace_is_active(trace_id):
        trace_file.write('-- %s: %s' % (trace_id, message))
        if insert_nl:
            trace_file.write('\n')
        trace_file.flush()


trace_fct = dont_print_trace
trace_file = sys.stdout
to_be_traced = {}
activate_trace_id_substrings = 0

def trace_is_active(trace_id):
    global to_be_traced, activate_trace_id_substrings
#    print '-- debug.trace_is_active: trace_id=%s' % trace_id

    if to_be_traced == 'all':
        return 1
    
    if not activate_trace_id_substrings and to_be_traced.has_key(trace_id):
        return 1

    if activate_trace_id_substrings:
        for a_substring in to_be_traced.keys():
            if string.find(trace_id, a_substring) >= 0:
                return 1

    return 0
        

def trace(trace_id, message):
    #
    # This may look like we have an unnecessary level of indirection, i.e.
    # why not call trace_fct() direction instead of trace()?
    # The problem there is that if we write "from debug import trace_fct",
    # then we import whatever function trace_fct happens to refer to at the
    # time of import, NOT AT THE TIME OF EXECUTION. So even if we invoke
    # config_traces() to change trace_fct after import, we will still be
    # invoking the old trace_fct(), not the new one.
    #
    trace_fct(trace_id, message)

def config_traces(print_to=None, status=None, active_traces=None,
                  allow_trace_id_substrings=None):
    """Configures what traces are printed, and where"""

    global trace_fct, trace_file, to_be_traced, activate_trace_id_substrings

    #
    # trace_fct is a function that we set on the fly. It's never defined explicitly
    # through a *def* statement
    #
    if status:
        if status == 'on':
            trace_fct = print_trace
        else:
            trace_fct = dont_print_trace

    if print_to:
        trace_file = print_to

    if active_traces:
        to_be_traced = active_traces

    if allow_trace_id_substrings != None:
        activate_trace_id_substrings = allow_trace_id_substrings
        
#    print '-- debug.config_traces: upon exit, trace_fct=%s, trace_file=%s, to_be_traced=%s, activate_trace_id_substrings=%s' % (trace_fct, trace_file, to_be_traced, activate_trace_id_substrings)


    
