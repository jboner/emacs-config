#############################################################################
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
# placing apart, for prevention of circular imports
from WhatCanISay import WhatCanISay
from actions_gen import Action
from debug import trace

class ActionWhatCanISay(Action):
    """give what can I say info...
    
    **INSTANCE ATTRIBUTES**
        
    *none*
        
    CLASS ATTRIBUTES**
        
    *none* -- 
    
    Moves to instance of class WhatCanISay, written mainly by Quintijn Hoogenbooms
    This instance makes a current language aware overview of LSAs (and later) CSCs.
    The info is presented in HTML format and opened with webbrowser in the default browser.

    Default is info on current language, all commands, except common like punctuation

    When curr_context is set, the current context will be checked, only the valid commands
    are given (interpreted "by hand"), because the real interpreter is not asked for.
    But if a CSC matches a possible LSA with the same spoken_form is ignored.
    If more (Context, Action) combinations match, they are both given, but in practice the first will
    be executed.

    When all_lang is set (may not together with  curr_context), a full website with all languages AND
    all common commands (like punctuation) is produced..
    
    """

    def __init__(self, buff_name=None, **kwargs):
        self.curr_context = None
        self.all_lang = None
        for k,v in kwargs.items():
            setattr(self, k, v)
        self.deep_construct(ActionWhatCanISay, \
                            {}, \
                                {}, \
                            {})
                            
    def doc(self):
        return 'Give actual command info.';

    def execute(self, app, cont, state = None):
        """See [Action.execute] for details.
        
        .. [Action.execute] file:///./Action.Action.html#execute"""
        
        manager = app.current_manager()
        trace('actions_wcisay.execute','curr_context: %s, all_lang: %s'% \
              (self.curr_context, self.all_lang))
        if manager:
            wciSay = WhatCanISay()
            wciSay.load_commands_from_interpreter(app, manager.interpreter(), app.language_name(),
                                                  curr_context=self.curr_context, all_lang=self.all_lang)
            wciSay.create_cmds()
            wciSay.create_html_pages()  
            wciSay.show_cmds()


