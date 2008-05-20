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

import util
import exceptions, re, string, sys, traceback, types

"""Functions for debugging purposes."""

def not_implemented(name):
    """Prints warning message when a stub function is called."""
    print "WARNING: stub function %s is not implemented yet!!!" % name
    if traces_are_on():
       trace_call_stack(name)

def virtual(name, instance=None):
    """Prints warning message when a virtual method is called."""
    mess = "WARNING: virtual method '%s'." % name
    if instance:
       mess = "%s Invoked on instance %s!!!" % (mess, instance)
    print mess
    if traces_are_on():
       print_call_stack()

def critical_warning(warn):
    """Prints a critical warning message.
    
    A critical warning usually indicates a bug in the code which should
    be reported and resolved, but in a circumstance where the best 
    immediate response is to ignore and continue running.  For example, 
    if an error is detected by an object destructor or during cleanup, it 
    may be advisable to proceed to free the rest of the resources used
    by the object."""
    sys.stderr.write(warn)
    notify_trace_listeners(warn)

def config_warning(warn):
    """Prints a warning message about a possible configuration error
    
    Configuration errors may be caused by the standard commands defined
    in vc_config, but could also be caused by user definitions in
    user_config.
    """
#   for now, just write to stderr.  Later, we may want to log this to a
#   file 
    sys.stderr.write(warn)



def print_call_stack(print_to_file=sys.stdout):
    """Prints the call stack.

    This is done by raising an exception, catching it and printing the
    traceback object. In Python 2, there is a more direct way of doing this.
    """
    print_to_file.write("\nCall stack was:\n")
    try:
        raise exceptions.Exception()
    except exceptions.Exception, err:        
        traceback.print_stack(file=print_to_file)
        print_to_file.write("\n\n")

def trace_call_stack(trace_id, location_id=None, print_to_file=sys.stdout):
    if not location_id:
       location_id = trace_id
    if tracing(trace_id) and trace_is_active(trace_id):
       print_to_file.write("-- %s: call stack at location '%s' is:\n" % 
       (trace_id, location_id))
       print_call_stack(print_to_file)

###############################################################################
# class introspection functions
###############################################################################

def what_class(instance):
    """Returns the class of an instance.

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
    
def isinstance_of_some_class(instance):
    trace('debug.isinstance_of_some_class', 'instance=%s, repr(what_class(instance))=%s' % (instance, repr(what_class(instance))))
    class_name = repr(what_class(instance))
    is_an_instance = 0
    if re.match('^\s*<\s*class\s+', class_name):
       is_an_instance = 1
    trace('debug.isinstance_of_some_class', 'returning is_an_instance=%s' % is_an_instance)
    return is_an_instance
        

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
    global to_be_traced, trace_listeners

    if trace_is_active(trace_id):
        trace = '-- %s: %s' % (trace_id, message)
        if insert_nl:
           trace = "%s\n" % trace
        notify_trace_listeners(trace)

def notify_trace_listeners(message, trace_id=None):
    global trace_listeners
    for a_trace_listener in trace_listeners: 
        try:
            a_trace_listener.on_trace(message)
        except exceptions.Exception, err:        
            # 
            # When debugging, sometimes 
            # 
            sys.stderr.write("Could not print trace unto the following trace listener: %s\ntrace id was: %s\ntrace message was: %s.\nReason for failure was: %s" % 
                             (a_trace_listener, trace_id, message, err))


def add_trace_listener(listener):
    global trace_listeners
    trace_listeners.append(listener)

class TraceListener:
    def __init__(self):
       pass
       
    def on_trace(self, message):
       virtual('TraceListener.on_trace')
       
class STDERR_TraceListener(TraceListener):
    def __init__(self):
         TraceListener.__init__(self)
         
    def on_trace(self, message):
         sys.stderr.write(message)



def traces_are_on():
   if trace_fct is print_trace:
      return 1
   else:
      return None


def tracing(trace_id):
    return trace_is_active(trace_id) and trace_fct != dont_print_trace

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
    if callable(message):
       mess_string = message()
    else: 
       mess_string = message
    trace_fct(trace_id, mess_string)

def config_traces(status=None, active_traces=None,
                  allow_trace_id_substrings=None):
    """Configures what traces are printed, and where"""

    global trace_fct, to_be_traced, activate_trace_id_substrings

    #
    # trace_fct is a function that we set on the fly. It's never defined explicitly
    # through a *def* statement
    #
    traces_are_on = status
    if status:
        if status == 'on':
            trace_fct = print_trace
        else:
            trace_fct = dont_print_trace


    if active_traces:
        to_be_traced = active_traces

    if allow_trace_id_substrings != None:
        activate_trace_id_substrings = allow_trace_id_substrings
        

trace_fct = dont_print_trace
traces_on = False
trace_listeners = [STDERR_TraceListener()]
to_be_traced = {}
activate_trace_id_substrings = 0

    
