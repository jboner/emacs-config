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

"""Context objects which are specific to Emacs"""

from Context import Context
import debug
import AppStateEmacs
import re

class ContEmacs(Context):
    """This context applies iif we are connected to Emacs."""

    def __init__(self, **attrs):
        super(ContEmacs, self).__init__(attrs)

       
    def _applies(self, app, preceding_symbol = 0):

       return isinstance(app, AppStateEmacs.AppStateEmacs)

    def scope(self):
        """returns a string indicating the scope of this context.
        Commands with more specific scopes are checked first.

        See Context for details of the recognized scopes

        **INPUTS**

        *none*

        **OUTPUTS**

        *STR* -- the string identifying the scope
        """
        return "buffer"

    def equivalence_key(self):
        """returns a key used to separate Context instances into
        equivalence classes.  Two contexts which are equivalent (i.e.
        share the same set of circumstances under which they apply)
        should have identical keys.  Two contexts which are not
        equivalent should have distinct keys.

        For example, two instances of ContPy should both return the same
        key.

        See Context for more details.

        **INPUTS**

        *none*

        **OUTPUTS**

        *STR* -- the key
        """
        return "Emacs"


class ContEmacsInBuffNamed(Context):
    """This context applies iif the cursor is in an Emacs buffer whose
    name matches *self.name*"""

    def __init__(self, name, **attrs):
        self.name = name
        super(ContEmacsInBuffNamed, self).__init__(attrs)
       
    def _applies(self, app, preceding_symbol = 0):
       answer = 0
       tmp_cont = ContEmacs()
       if tmp_cont.applies(app, preceding_symbol):
          buff_name = app.curr_buffer_name()
          if re.match(self.name, buff_name):
             answer = 1
          
       debug.trace('ContEmacsInBuffNamed._applies', 'buff_name=%s, returns answer=%s' % (buff_name, answer))
       
       return answer        

    def scope(self):
        """returns a string indicating the scope of this context.
        Commands with more specific scopes are checked first.

        See Context for details of the recognized scopes

        **INPUTS**

        *none*

        **OUTPUTS**

        *STR* -- the string identifying the scope
        """
        return "buffer"

    def equivalence_key(self):
        """returns a key used to separate Context instances into
        equivalence classes.  Two contexts which are equivalent (i.e.
        share the same set of circumstances under which they apply)
        should have identical keys.  Two contexts which are not
        equivalent should have distinct keys.

        For example, two instances of ContPy should both return the same
        key.

        See Context for more details.

        **INPUTS**

        *none*

        **OUTPUTS**

        *STR* -- the key
        """
        return "EmacsInBuffNamed-%s" % self.name

        
cont_emacs_in_minibuff = ContEmacsInBuffNamed("^\s*\*\s*Minibuf-[\d]+\s*\*\s*$")
cont_emacs_in_selection_buff = ContEmacsInBuffNamed("^\s*\*[\s\S]+\*\s*$")
