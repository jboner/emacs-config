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

"""Actions specific for Emacs"""

from actions_gen import *
import debug
import sr_interface
import time
import string

class ActionEmacsListBuffers(Action):
    """open the Emacs buffer list.

    **INSTANCE ATTRIBUTES**

    *none*
    """
    
    def __init__(self, **args_super):
        self.deep_construct(ActionEmacsListBuffers, 
                            {}, 
                            args_super, 
                            {})

    def execute(self, app, cont, state = None):
        """Execute the action.
        
        **INPUTS**
        
        [AppState] app -- Application on which to execute the action
        
        [Context] app_cont -- Context of the application that
        determines the parameters of how the action will be executed.

        [InterpState] state -- interface to the interpreter state
        
                
        **OUTPUTS**
        
        *none* -- 

        .. [AppState] file:///./AppState.AppState.html"""
        debug.trace('ActionEmacsListBuffers.execute', '** invoked')
        #
        # AD: This would probably be better handled by invoking 
        #     app.app_change_buffer(), but unfortunately, I couldn't
        #     find a programmatic way of replicating what happens in
        #     Emacs when you interactively invoke `switch-bo-buffer
        #     and then type tab. The problem is that I couldn't figure
        #     out how to display the list of buffers in a window that 
        #     disappears automatically when you select one of the buffers.
        #     With everything I tried, if you start out with a single
        #     window layout, you eventually ended up with a 2 windows
        #     layout.

        buffer_names = app.list_all_buffers()
        key_strokes='{Esc}xswitch-to-buffer{Enter}{Tab}{Esc}v'
        ActionTypeText(key_strokes = key_strokes).execute(app, cont, state)
        time.sleep(1)
        contents = string.join(buffer_names, '\n')
        ActionCompileSymbols("*Completions*", alt_contents = contents,
            langauge = 'file_names').execute(app=app, cont=cont, state=state)
        return
# Emacs should already have switched, but we need to tell the mediator
# to re-bind to this buffer
        debug.trace('ActionEmacsListBuffers.execute',
            'before change_buffer, buffer is %s' % app.curr_buffer_name())
        success = app.change_buffer('*Completions*')
        debug.trace('ActionEmacsListBuffers.execute',
            'after change_buffer, success = %d, buffer is %s' % success, app.curr_buffer_name())


    def doc(self):
        """Returns a documentation string for the action.
        
        **INPUTS**
        
        *none* -- 
        

        **OUTPUTS**
        
        *none* -- 
        """
        return "Opens the Emacs buffer list."
