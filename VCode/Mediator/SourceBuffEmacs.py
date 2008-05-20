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
# (C)2001, National Research Council of Canada
#
##############################################################################

"""State information for an external source buffer connected to
VoiceCode via a messaging protocol."""

import messaging, sb_services, SourceBuffMessaging, cont_gen, debug, sr_interface
import re

class SourceBuffEmacs(SourceBuffMessaging.SourceBuffInsertIndentMess):
    
    """Interface to a buffer of the Emacs editor.
    
    **INSTANCE ATTRIBUTES**

    [SB_ServiceLangMessaging] *lang_srv* -- Language service used to know the
    programming language of a source file.
    
    Note that the *app* attribute (defined in [SourceBuff])
    needs to be a subclass of [AppStateEmacs]

    **CLASS ATTRIBUTES**
    
    *none* --

    ..[SourceBuff] file:///./SourceBuff.SourceBuff.py
    ..[AppStateEmacs] file:///./AppStateEmacs.AppStateEmacs.html"""
    
    def __init__(self, **attrs):
        self.init_attrs({'lang_srv': sb_services.SB_ServiceLangMessaging(buff=self)})
        self.deep_construct(SourceBuffEmacs,
                            {},
                            attrs
                            )
        self.add_owned('lang_srv')
#        self._maybe_disable_cache()

    def _maybe_disable_cache(self):
        """disable caching for certain buffers where Emacs's change tracking is buggy.

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        if self.name() == "*Buffer List*":
           self.use_cache = 0
    

    def language_name(self):
        """Returns the name of the language a file is written in.

        This would be better handled at the Emacs level, but I haven't
        figured out how to find the language of a buffer in Emacs (AD).
        
        **INPUTS**
        
        *none*
        
        **OUTPUTS**
        
        *STR* -- the name of the language
        """

        return self.lang_srv.language_name()

#
# AD: This is not needed for now, because we decided not to speech enable
#     the minibuffer for now. But I leave it there in case we ever decide
#     to speech enable the minibuffer.
#     Note that this was working only partly... it was causing some problems
#     with correction.
#
#    def insert_indent(self, code_bef, code_after, range = None):
#        """Send an insert_indent message, unless we are in the minibuffer and the code
#        contains newlines.
#        
#        For some reason, if we send an insert_indent to the minibuffer and the code to
#        be inserted contains some newlines, Emacs never replies with insert_indent_resp 
#        (although it does not crash).
#        
#        As a temporary workaround for this, we do the following:
#        
#           * if the code is just a \n, then send it through playString instead.
#           * if the code contains \n plus more, substitute the \n by spaces and
#             send the code through a regular message."""
#
#        debug.trace('SourceBuffEmacs.insert_indent', '** invoked')                
#        
#        if cont_gen.ContEmacsInMinibuffer().applies(self.app):
#           debug.trace('SourceBuffEmacs.insert_indent', '** IN minibuff')                
#           if (re.match('^\s*\n\s*$', code_bef) and re.match('^\s*$', code_after) or
#               re.match('^\s*$', code_bef) and re.match('^\s*\n\s*$', code_after)):
#              #
#              # Code to be inserted is just '\n'. Play that key sequence directly
#              # into the current window
#              #
#              debug.trace('SourceBuffEmacs.insert_indent', '** playing newline in minibuff')        
#              sr_interface.send_keys('\n')                                       
#              return
#           else:
#              debug.trace('SourceBuffEmacs.insert_indent', '** substituting newlines in code')                           
#              code_bef = re.sub('\n', ' ', code_bef)
#              code_after = re.sub('\n', ' ', code_after)           
#        
#              
#        SourceBuffMessaging.SourceBuffInsertIndentMess.insert_indent(self, code_bef, code_after, range)                      
#
#           
#    def insert(self, text, range = None):            
#        """Send an insert message, unless we are in the minibuffer and the code
#        contains ewlines.
#        
#        For some reason, if we send an insert_indent to the minibuffer and the code to
#        be inserted contains some newlines, Emacs never replies with insert_indent_resp         (although it does not crash).
#        
#        As a temporary workaround for this, we do the following:
#        
#           * if the code is just a \n, then send it through playString instead.
#           * if the code contains \n plus more, substitute the \n by spaces and
#             send the code through a regular message."""
#
#
#        if cont_gen.ContEmacsInMinibuffer().applies(self.app):
#           debug.trace('SourceBuffEmacs.insert', '** IN minibuff')                
#           if re.match('^\s*\n\s*$', text):
#              #
#              # Code to be inserted is just '\n'. Play that key sequence directly
#              # into the current window
#              #
#              debug.trace('SourceBuffEmacs.insert', '** playing newline in minibuff')        
#              sr_interface.send_keys('\n')                                       
#              return
#           else:
#              debug.trace('SourceBuffEmacs.insert', '** substituting newlines in code')                           
#              text = re.sub('\n', ' ', text)
#
#        SourceBuffMessaging.SourceBuffInsertIndentMess.insert(self, text, range)
           

###################################################################
# Emacs-specific implementation of methods for code syntax navigation
###################################################################

    def syntax_nav_supported(self):
        """
        Indicates whether this SourceBuff instance supports syntax
        navigation (either in the mediator or through the external
        editor

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        return self.language_name() == 'C'

    def find_matching(self, direction = 1):
        """
        Finds a matching brace/bracket/parenthesis.

        NOTE: this method does not find matching quotes, or any other
        character where the opening and closing characters are
        identical

        ** INPUTS **

        *INT direction* -- direction of the search.  Direction = 1
        means to search forward for the character matching the one at
        the current cursor position.  Direction = minus 1 means to
        search backward for the character matching the one before the
        current cursor position.

        ** OUTPUTS **

        *INT* -- the position of the matching character, or if the
        character adjacent to the cursor was not a bracket, brace, or
        parenthesis, or if no matching character could be found
        """
        self.app.talk_msgr.send_mess('find_matching', 
            {'buff_name': self.buff_name, 'direction': direction})
        response = self.app.talk_msgr.get_mess(expect = ['find_matching_resp'])
        return messaging.messarg2int(response[1]['value'])

    def beginning_of_statement(self):
        """Finds the location of the beginning of the current
        statement

        NOTE: initially, this method maybe implemented using the
        external editor, said the exact definition of the current
        statement and where it starts may vary.

        ** INPUTS **

        *none*

        ** OUTPUTS **

        *INT* -- the position of the beginning of the statement found
        at the cursor position
        """
        self.app.talk_msgr.send_mess('beginning_of_statement',
            {'buff_name': self.buff_name})
        response = \
            self.app.talk_msgr.get_mess(expect = ['beginning_of_statement_resp'])
        return messaging.messarg2int(response[1]['value'])
    
