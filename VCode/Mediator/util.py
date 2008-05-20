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

"""Various utility functions"""

import getopt, os, re, stat, sys, time, types
import sys
if sys.platform =='win32':
    global winsound
    import winsound

import os.path

import vc_globals

def dict_merge(dict1, dict2):
    """Merges two dictionaries
    
    Merges *{ANY: ANY} dict1* and *{ANY: ANY} dict2* and returns the
    result *{ANY: ANY} dict3*.

    If a key exists in both *dict1* and *dict2*, the value in *dict1*
    will be used for *dict3*
    """
    dict3 = dict2
    for an_item in dict1.items():
        key, val = an_item
        dict3[key] = val
    return dict3



def gopt(opt_defs, cmd=sys.argv[1:]):
    """High level wrapper around *getopt.getop*.

    *removes first argument from *cmd* when parsing from *sys.argv*
        
    *returned options are stored in a dictionary

    *dashes ('-') are removed from the option name in that dictionary

    *returns None and outputs error messages if invalid option

    *allows to define default values for options
        
    **INPUTS**
        
    *[STR]* cmd=sys.argv[1:] -- list of options and arguments to be parsed. 
        
    *[STR, ANY, ...]* opt_defs -- defines the valid options (short and
    long). The list is an alternate sequence of option name and
    default value. If the name ends with *=*, it means the option
    requires a value. If the name is a single letter, it's a short
    option. The defaul value is compulsory, even for options that
    don't require a value (can be used to set the switch to on or
    off by default).
        

    **OUTPUTS**
        
    *opts, args* -- *opts* is a dictionary of options names and
    values. *args* is the list of arguments.
    """

    opt_dict = {}
    args = []    
    
    #
    # Set default values of options
    #
    index = 0
    while (index < len(opt_defs)):
        opt_name = opt_defs[index]
        opt_default = opt_defs[index + 1]
        opt_name = re.match('^(.*?)(=*)$', opt_name).groups()[0]        
        opt_dict[opt_name] = opt_default
        index = index + 2
#    print '-- util.gopt: initialised opt_dict=%s' % repr(opt_dict)


    #
    # Set options specifications to be used by getopt.
    #
    short_opts = ''
    long_opts = []

    requires_val = {}
    is_long = {}    
    index = 0
    while (index < len(opt_defs)):
        opt_name = opt_defs[index]
        opt_default = opt_defs[index + 1]
        index = index + 2
        match = re.match('^(.)(.*?)(=*)$', opt_name)
        opt_name = match.group(1)+ match.group(2)
        if (match.group(2) != ''):
            is_long[opt_name] = 1
        if (match.group(3) != ''):
            requires_val[opt_name] = 1
            
        if is_long.has_key(opt_name):
            long_opts = long_opts + [opt_name + match.group(3)]
        else:
            short_opts = short_opts + opt_name
            if requires_val.has_key(opt_name):
                short_opts = short_opts + ':'

    #
    # Parse the command line options
    #
#    print '-- util.gopt: calling getopt with cmd=%s, short_opts=%s, long_opts=%s' % (repr(cmd), repr(short_opts), repr(long_opts))                
    options, args = getopt.getopt(cmd, short_opts, long_opts)    
#    print '-- util.gopt: options=%s, args=%s' % (repr(options), repr(args))


    #
    # Assign parsed values to the options dictionary.
    #
#    print '-- util.gopt: is_long=%s, requires_val=%s' % (repr(is_long), repr(requires_val))
    for an_opt in options:
        opt_name = an_opt[0]
        a_match = re.match('^(-*)(.*)$', opt_name)
        opt_name = a_match.group(2)
        if not requires_val.has_key(opt_name):
            #
            # getopt.getopt returns None as the value for BOOLEAN options
            # but we want it to be 1, otherwise it makes it look like the
            # options was off
            #
            # In getopt.getopt, that didn't matter because the mere presence
            # of the option name indicates it is on.
            #
            # In util.gopt, all options have an entry in the returned
            # dictionary, and its value indicates whether it's on or off
            #
            opt_val = 1
        else:
            opt_val = an_opt[1]
        opt_dict[opt_name] = opt_val

#    print "-- gopt: opt_dict=%s, args=%s" % (str(opt_dict)    , str(args))        
    return opt_dict, args

###############################################################################
# list processing
###############################################################################

def remove_occurences_from_list(item, list, max_occurs=None):
   num_found = 0
   new_list = []
   ii = 0
   for ii in range(len(list)):
      if list[ii] == item:
         num_found = num_found + 1
         if max_occurs != None and num_found >= max_occurs:
            break
      else:
         new_list.append(list[ii])
      
   ii_rest = ii + 1   
   if ii_rest < len(list):
      new_list = new_list + list[ii_rest:]

      
   return new_list


###############################################################################
# file system
###############################################################################


def last_mod(f_name):
    """Returns the time at which a file was last modified.

    *STR f_name* is the path of the file.

    if *f_name* doesn't exist, returns 0.
    """

    try:
        stats = os.stat(f_name)
        time = stats[stat.ST_MTIME]
    except OSError:
        time = 0
        
    return time


###############################################################################
# For redirecting STDIN
###############################################################################

#
# Use this temporary file to send commands to the process' own stdin
#
redirected_stdin_fname = vc_globals.tmp + os.sep + 'user_input.dat'
redirected_stdin = open(redirected_stdin_fname, 'w')

def stdin_read_from_string(string):
    """Redirects process' own stdin so it reads from a string

    **INPUTS**
    
    *[STR] string* -- String from which to read stdin

    **OUTPUTS**

    *FILE old_stdin* -- Stream that stdin was originally assigned to.
    """ 

    global redirected_stdin, redirected_stdin_fname
    
    old_stdin = sys.stdin

    #
    # Close temporary file in case it was opened by a previous call to
    # stdin_read_from_string
    #
    redirected_stdin.close()

    #
    # Write the string to temporary file
    #
    redirected_stdin = open(redirected_stdin_fname, 'w')
    redirected_stdin.write(string)
    redirected_stdin.close()

    #
    # Open temporary file and assign it to stdin
    #
    redirected_stdin = open(redirected_stdin_fname, 'r')
    sys.stdin = redirected_stdin

    return old_stdin

###########################################################################
# Identifying instances of basic types
###########################################################################

def islist(instance):
    """Returns true iif *instance* is a list."""

    return isinstance(instance, types.ListType)
    
def istuple(instance):
    """Returns true iif *instance* is a tuple."""

    return isinstance(instance, types.TupleType)

def issequence(instance):
    return islist(instance) or istuple(instance)

def isfunction(instance):
    return isinstance(instance, types.FunctionType)
   

###############################################################################
# path manipulation 
###############################################################################

def full_split(path):
    head, tail = os.path.split(path)
    if head == '': return [tail]
    if head == path: return [path]
    l = full_split(head)
    l.append(tail)
    return l

def find_common(a, b):
    n = min(len(a), len(b))
    for i in range(n):
        if a[i] != b[i]:
            return i
    return n

def remove_common(first, second):
    f = full_split(first)
    s = full_split(second)
    n = find_common(f,s)
    return f[n:]

def common_path(first, second):
    f = full_split(first)
    s = full_split(second)
    n = find_common(f,s)
    return f[:n]

def relative_name(path, head, prefix=''):
    common = common_path(path, head)
    whole_head = full_split(head)
    if (whole_head != common):
        return path
    p = []
    if (prefix != ''):
        p.append(prefix)
    p.extend(remove_common(path,head))
    return apply(os.path.join, p)

def replace_all_within_VCode(text, prefix='%VCODE_HOME%' ):
    return text.replace(vc_globals.home, prefix)

def within_VCode(path):
#    return relative_name(path, vc_globals.home)
    return relative_name(path, vc_globals.home, prefix='%VCODE_HOME%')
#    return relative_name(path, vc_globals.home, 
#        prefix = os.path.basename(vc_globals.home))


###############################################################################
# miscellaneous
###############################################################################


def bell(length=3):
    """Plays a bell sound for a time proportional to INT length.
    """
    
    bell_string = ''
    for ii in range(length):
       bell_string = bell_string + '\a'
# use sys.__stderr__ so that the user will hear the bell even if stderr
# is redirected by regression testing
    sys.__stderr__.write(bell_string)
    sys.__stderr__.flush()

# AD: Uncomment once we have upgraded to wxPython 2.5.    
#import wx
#def wxPython_is_unicode_build():
#   if 'unicode' in wx.PlatformInfo:
#      print "wxPython was built in Unicode mode"
#      return True
#   else:
#      print "wxPython was built in ANSI mode"
#      return False

