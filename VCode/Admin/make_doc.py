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

"""Script used to refresh the HTML documentation for the VoiceCode Python modules.

Usage: python make_doc.py files

files: names of .py files for which to refresh the documentation. If files is not specified, then documentation for all .py files is refreshed.

Example: python make_doc.py Object.py
"""

import pythondoc.pythondoc
import glob, os, posixpath, re, sys, string
import util, vc_globals

regs_file_name = None

def fix_references(doc_dir):
   """For some reason, internal references must be specified as:

   \.\. [anchor] file:///./this_file.html#internal_ID (for internal refs)
   or
   \.\. [anchor] file:///some_file.html#ID (for external refs)
   

   instead of

   \.\. [anchor] #internal_ID or \.\. [anchor] some_file.html#ID

   The later causes pythondoc to choke on the definition (note that
   the syntax "anchor":#internal_ID doesn't work either, because
   pythondoc inserts it as is).

   The problem is that while pythondoc works with
   file:///./this_file#internal_ID, neither Netscape nor IE will find
   the file unless they are started from the Doc directory.

   To deal with this problem, *fix_internal_references* substitutes
   file:///./ and file:/// for a null string in all the Doc/*.html files.   
   """

   files = glob.glob(doc_dir + os.sep + '*.html')
   for a_file_name in files:

      #
      # Read the file
      #
      a_file = open(a_file_name, 'r')
      a_file_content = a_file.read()
      a_file.close()

      #
      # Substitute 'file:///./' -> ''
      #
      reg_internal_ref = 'file:\\/\\/\\/.\\/'
      reg_external_ref = 'file:\\/\\/\\/'
      (a_file_content, num_subst1) = re.subn(reg_internal_ref, '', a_file_content)
      (a_file_content, num_subst2) = re.subn(reg_external_ref, '', a_file_content)

      #
      # Don't resave the file if hasn't change, in order to preserve its date.
      #
      if num_subst1 + num_subst2:
         a_file = open(a_file_name, 'w')
         a_file.write(a_file_content)
         a_file.close()


def find_pydoc_path(names=None):
    """search through the python import path for the pydoc script

    **INPUTS**

    *[STR]* names -- list of possible names for the script (defaults to
        ['pydoc.py', 'pdoc.py']

    **OUTPUTS**

    *STR* -- full path of the script, or None if it couldn't be found.
    """
    if not names:
        names = ['oldpydoc.py', 'pdoc.py', 'pydoc.py']
    for name in names:
        for directory in sys.path:
            if os.path.exists(directory):
                if name in os.listdir(directory):
                    return os.path.join(directory, name)
    return None

def quit():
    """central quitting routine

    **INPUTS**

    *none*

    **OUTPUTS**

    *none*
    """
    sys.exit()

def runme():

    #
    # Path of the pythondoc or pydoc.py script
    #
    pydoc_path = find_pydoc_path()
    if not pydoc_path:
        sys.stderr.write("unable to locate pydoc script\n")
        quit()
    sys.stderr.write("pydoc was found in\n" + pydoc_path + "\n")
       
    #
    # Get names of py files to document
    #
    #    glob_pattern = posixpath.expandvars('$VCODE_HOME' + os.sep + 'Mediator' + os.sep + '*.py')
    glob_pattern = posixpath.expandvars('$VCODE_HOME' + os.sep + '*' + os.sep + '*.py')
    files = glob.glob(glob_pattern)

    #
    # Remove files that don't match name patter
    #
    slow = 0
    if sys.argv: 
        opts, args = util.gopt(['s', None])
    # parse command line options and arguments
        if opts:
            if opts['s']:
                slow = 1
        regs_file_name = args
    if regs_file_name:
        orig_files = files
        files = []
        for a_file in orig_files:
            for a_reg in regs_file_name:
                a_reg = '[\s\S]*\\' + os.sep + a_reg + '$'
                if re.match(a_reg, a_file):
                    files = files + [a_file]
                    break

    #
    # Remove make_doc.py from files list
    #
#     this_file_name = posixpath.expandvars('$VCODE_HOME' + os.sep + 'Admin' + os.sep + 'make_doc.py')
#     this_file_ind = files.index(this_file_name)
#     files = files[:this_file_ind - 1] + files[this_file_ind + 1:]
   
      
    # Not needed anymore
    # modules = map(pythondoc.pythondoc.path2module, files)
    
    #
    # Set format to HTML4
    #
    #  formats = []
    #  pythondoc.pythondoc.add_formatter('HTML4', formats)
    
    #
    # Set documentation directory to $VCODE_HOME/Doc/Modules
    #
    doc_dir = posixpath.expandvars(vc_globals.doc_modules)
   
    #  pythondoc.pythondoc._options.add_value('main_directory', doc_dir)
    #  pythondoc.pythondoc._options.add_value('main_index', 1)
   
    # for a_module in modules:
    #     sys.stdout.write("   %s\n" % a_module)
    #     pythondoc.pythondoc.generate_pages(modules, formats)
       
    #
    # Generate the HTML doc for each module
    #
    cmd_line = 'e:\python\python.exe ' + pydoc_path + ' -i -f HTML4 -d ' + doc_dir + ' '
    for a_file in files:
           
        #
        # Check to see if HTML doc needs to be refreshed
        #
        a_match = re.match('.*?([^\\' + os.sep + ']*)\.py$', a_file)
        module_name = a_match.groups()[0]
        html_file = doc_dir + os.sep + module_name + '.html'

        if (util.last_mod(html_file) < util.last_mod(a_file)):
            print "   Updating documentation for module '%s'...\n" % module_name
            cmd = cmd_line + a_file
            os.system(cmd)
            if slow:
                print "   That was module '%s'...\n\n" % module_name
                raw_input("press enter to continue")

    #
    # Fix problem with references
    #
    fix_references(doc_dir)


if (__name__ == '__main__'):   
    runme()










