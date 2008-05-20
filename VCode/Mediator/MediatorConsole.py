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

import sys
import debug
import Object, vc_globals
from wxPython.wx import *

"""Defines abstract interface for the mediator GUI console and all other
GUI windows and dialog boxes

**MODULE VARIABLES**


"""

class DismissModalEvent(Object.Object):
    """abstract interface for sending a message to a modal dialog to 
    dismiss it.

    **INSTANCE ATTRIBUTES**

    *none*

    **CLASS ATTRIBUTES**

    *none*
    """
    def __init__(self, **args):
        self.deep_construct(DismissModalEvent,
                            {},
                            args)

    def dismiss(self):
        """send the message, and return synchronously

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        debug.virtual('DismissModalEvent.dismiss')
   

class MediatorConsole(Object.OwnerObject):
    """
    **INSTANCE ATTRIBUTES**

    *NewMediatorObject mediator* -- the mediator which owns this console

    *WinGramFactory gram_factory* -- the grammar factory used to add
    speech grammars to dialog boxes

    *INT main_frame_handle* -- the window-system specific ID for the
    main frame of the mediator application

    *[DismissModalEvent] dismiss_events -- a stack of
    DismissModalEvents, one  for each modal dialog box currently shown
    on top of the console.

    *WinSystem win_sys -- WinSystem interface to
    window-system specific functions

    *BOOL* testing -- flag indicating whether we are currently
    performing regression tests

    **CLASS ATTRIBUTES**
    
    *none* 
    """
    def __init__(self, main_frame_handle, win_sys, **attrs):
        """
        **INPUTS**

        *INT main_frame_handle* -- the window-system specific ID for the
        main frame of the mediator application

        *WinSystem win_sys -- WinSystem interface to
        window-system specific functions
        """
        self.deep_construct(MediatorConsole,
                            {
                             'main_frame_handle': main_frame_handle,
                             'dismiss_events': [],
                             'win_sys': win_sys,
                             'mediator': None,
                             'testing': 0,
                             'gram_factory': None
                            },
                            attrs)
        self.name_parent('mediator')
        self.win_sys.set_main_frame_handle(main_frame_handle)

    def starting_tests(self):
        """method used by NewMediatorObject to notify us that it is
        about to start regression testing

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        pass

    def finished_tests(self):
        """method used by NewMediatorObject to notify us that it is
        done with regression testing

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        pass

    def store_foreground_window(self):
        """detect the current foreground window, and store it in a
        WasForegroundWindow object, so that the window can later
        be restored to the foreground

        **INPUTS**

        *none*

        **OUTPUTS**

        *WasForegroundWindow* -- the object which can be used to restore
        the window to the foreground
        """
        return self.win_sys.store_foreground_window()

    def set_mediator(self, mediator):
        """assigns a parent mediator to the console

        **INPUTS**

        *NewMediatorObject mediator* -- the parent mediator which will
        own the console

        **OUTPUTS**

        *none*
        """
        self.mediator = mediator

    def user_message(self, message, instance = None):
        """displays a user message (usually on a MediatorConsole status 
        line, but Natspeak-style tooltips might also be a possibility)

        **INPUTS**

        *STR message* -- the message

        *STR instance_name* -- the editor from which the message
        originated, or None if it is not associated with a specific
        editor.

        **OUTPUTS**

        *BOOL* -- true if the MediatorConsole implementation has a means
        of displaying user messages 
        """
        return 0

    def message_box(self, message):
        """displays an error or warning message in a message box

        **INPUTS**

        *STR message* -- the message

        **OUTPUTS**

        *none*
        """
        debug.virtual('MediatorConsole.message_box')

    def set_gram_factory(self, gram_factory):
        """used by NewMediatorObject to specify a WinGramFactory which the 
        console can use to speech-enable dialog boxes

        **INPUTS**

        *WinGramFactory gram_factory* -- the grammar factory used to add
        speech grammars to dialog boxes

        **OUTPUTS**

        *none*
        """
        self.gram_factory = gram_factory

    def already_modal(self):
        """does the console already have a modal dialog running?

        **INPUTS**

        *none*

        **OUTPUTS**
        
        *BOOL* -- true if a modal dialog is active
        """
        if self.dismiss_events:
            return 1
        return 0

    def push_modal(self, dismiss_event):
        """push a DismissModalEvent onto the stack before showing the
        corresponding modal dialog

        **INPUTS**

        *DismissModalEvent dismiss_event* -- DismissModalEvent which can be used
        to dismiss the modal dialog

        **OUTPUTS**

        *none*
        """
        self.dismiss_events.append(dismiss_event)

    def pop_modal(self):
        """pops a DismissModalEvent off the stack after the
        corresponding modal dialog has returned

        **INPUTS**

        *none*

        **OUTPUTS**

        *BOOL* -- true if there was a modal dialog on the stack
        """
        if self.already_modal():
            del self.dismiss_events[-1]
            return 1
        return 0

    def dismiss_modal(self):
        """dismisses the topmost modal dialog box currently shown by the
        console.  Control will return to the next topmost dialog, 
        which is expected to return as well, and so on down the line

        **INPUTS**

        *none*

        **OUTPUTS**
        
        *BOOL* -- true if the modal dialog box was sucessfully dismissed
        (or if there wasn't one to start with)
        """
        if not self.already_modal():
            return 1
        bye = self.dismiss_events[-1]
        bye.dismiss()
# don't pop the event off the stack, because the method which created
# the dialog should do this when control returns to it on dismissal of
# the dialog

    def copy_user_config(self, target, directory):
        """prompt the user for the sample user configuration file to
        copy to the target path, and copy the file

        **INPUTS**

        *STR target* -- the path of the default user configuration file

        *STR directory* -- the initial directory in which to look for a
        sample configuration file to copy

        **OUTPUTS**

        *BOOL* -- true if a file was selected and copied to the target
        path
        """
        debug.virtual('MediatorConsole.copy_user_config')

    def correct_utterance(self, editor_name, utterance, 
        can_reinterpret, should_adapt = 1):
        """Store the current foreground window,
        display a correction box for correction of a complete, recent
        utterance, accept user corrections, allow the user to
        approve or cancel, and adapt the speech engine.  Finally, restore
        the original window to the foreground

        **INPUTS**

        *STR editor_name* -- name of the editor instance

        *SpokenUtterance utterance* -- the utterance itself

        *BOOL can_reinterpret* -- flag indicating whether the utterance
        could be reinterpreted upon correction, allowing the correction
        box to give some visual feedback to the user to indictate this.
        Whether the utterance can actually be reinterpreted may change
        between the call to this method and its return, so there is no
        guarantee that reinterpretation will take place.

        *BOOL should_adapt* -- flag indicating whether correct_utterance
        should adapt the speech engine according to user corrections (if
        the user approves), or if the caller will handle that later.

        **OUTPUTS**

        *BOOL* -- true if the user made changes and approved them
        """
        editor_window = self.store_foreground_window()
        ok = self.show_correction_box(editor_name, utterance,
            can_reinterpret, should_adapt = should_adapt)
        editor_window.restore_to_foreground()
        return ok
        

    def show_correction_box(self, editor_name, utterance, 
        can_reinterpret, should_adapt = 1):
        """display a correction box for correction of a complete, recent
        utterance, accept user corrections, allow the user to
        approve or cancel, and adapt the speech engine.

        **INPUTS**

        *STR editor_name* -- name of the editor instance

        *SpokenUtterance utterance* -- the utterance itself

        *BOOL can_reinterpret* -- flag indicating whether the utterance
        could be reinterpreted upon correction, allowing the correction
        box to give some visual feedback to the user to indictate this.
        Whether the utterance can actually be reinterpreted may change
        between the call to this method and its return, so there is no
        guarantee that reinterpretation will take place.

        *BOOL should_adapt* -- flag indicating whether correct_utterance
        should adapt the speech engine according to user corrections (if
        the user approves), or if the caller will handle that later.

        **OUTPUTS**

        *BOOL* -- true if the user made changes and approved them
        """
        debug.virtual('MediatorConsole.show_correction_box')

    def correct_recent(self, editor_name, utterances):
        """Store the current foreground window,
        display a correct recent dialog box for to allow the user to 
        select a recent utterance to correct.  Finally, restore
        the original window to the foreground


        **INPUTS**

        *STR editor_name* -- name of the editor instance

        *[(SpokenUtterance, BOOL)] utterances* -- the n most recent dictation 
        utterances (or all available if < n), sorted most recent last, 
        with corresponding flags indicating if the utterance can be 
        undone and re-interpreted, or None if no utterances are stored.

        **OUTPUTS**

        *[INT]* -- the utterance numbers of 
        those utterances which were corrected by the user, or None if
        none were corrected
        """
        editor_window = self.store_foreground_window()
        changed = self.show_recent_utterances(editor_name, utterances)
        editor_window.restore_to_foreground()
        return changed
        
    def show_recent_utterances(self, editor_name, utterances):
        """display a correct recent dialog box for to allow the user to 
        select a recent utterance to correct

        **INPUTS**

        *STR editor_name* -- name of the editor instance

        *[(SpokenUtterance, BOOL)] utterances* -- the n most recent dictation 
        utterances (or all available if < n), sorted most recent last, 
        with corresponding flags indicating if the utterance can be 
        undone and re-interpreted, or None if no utterances are stored.

        **OUTPUTS**

        *[INT]* -- the utterance numbers of 
        those utterances which were corrected by the user, or None if
        none were corrected
        """
        debug.virtual('MediatorConsole.show_recent_utterances')

    def reformat_recent(self, editor_name, symbols):
        """Store the current foreground window,
        display a reformat recent dialog box  to allow the user to 
        select a recent symbol to reformat.  Finally, restore
        the original window to the foreground


        **INPUTS**

        *STR editor_name* -- name of the editor instance

        *[SymbolResult] symbols* -- the n most recent dictated symbols
        sorted most recent last. We assume that all those symbols
        are in an utterance that can be reinterpreted.
        
        **OUTPUTS**

        *none*
        """
        editor_window = self.store_foreground_window()
        reformatted_symbols = self.show_recent_symbols(editor_name, symbols)
        editor_window.restore_to_foreground()
        return reformatted_symbols

    def show_recent_symbols(self, editor_name, symbols):
        """display a dialog box with recent symbols to allow the user to 
        select a recent symbol to reformat

        **INPUTS**

        *STR editor_name* -- name of the editor instance

        *[SymbolResult] symbols* -- the n most recent dictated symbols
        sorted most recent last. We assume that all those symbols
        are in an utterance that can be reinterpreted.

        **OUTPUTS**

        *none*
        """
        debug.virtual('MediatorConsole.show_recent_symbols')


    def reformat_nth_symbol(self, editor_name, symbol):
        """Store the current foreground window,
        display a reformatting box for reformatting a recent
        symbol, accept user reformatting, allow the user to
        approve or cancel.  Finally, restore
        the original window to the foreground

        **INPUTS**

        *STR editor_name* -- name of the editor instance

        *SymbolResult symbol* -- the symbol itself

        **OUTPUTS**

        *BOOL* -- true if the user made changes and approved them
        """
        editor_window = self.store_foreground_window()
        ok = self.show_reformatting_box(editor_name, symbol)
        editor_window.restore_to_foreground()
        return ok


    def correct_symbol_results(self, instance_name, reformatted_symbols):
       self.mediator.correct_symbol_results(instance_name, reformatted_symbols)

    def raise_active_window(self):
        """makes the active window (within the current process) the
        foreground one (for the system)

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        debug.virtual('MediatorConsole.raise_active_window')
    
class DlgModelView(Object.OwnerObject):
    """Symbol Model-View class for implementing dialogs.
    
    This uses a simple model-view architecture for dialogs. This
    allows a cleaner separation between business logic and the
    rendering of the dialog, and also, it makes automated unit
    testing easier.
    
    The view is responsible solely for:
      - rendering data graphically upon request from the model
      - intercepting user event and dispatching them to the model
      - note that the view is NOT responsible for coordinating the 
        content of the various fields it displays
      - responding to requests from the model regarding what it is
        currently displaying in a particular field (for assertions in
        unit testing)
      - programmatically simulating user events upon request from the
        model (for unit testing)
      
    The model is responsible for:
      - sending rendering requests to the view
      - responding to requets from the view made in response to
      - coordinating the content of the various fields of the view 
        so that they are always in sync
      - requesting the view to simulate user events (for regression
        testing)

    By convention, any method whose name looks like do_X, is a method
    which is meant to programmatically simulate user action X. Any
    method whose name looks like on_X is a method which is invoked
    when the user does action X.
    
    **INSTANCE ATTRIBUTES**

    *ViewLayer* view_layer -- The presentation layer for this dialog
    (note this may be *self* if presentation layer and model
    layer are rolled into a single same object). 

    **CLASS ATTRIBUTES**
    
    *none* --    
    """
    def __init__(self, **args_super):
        self.deep_construct(DlgModelView, 
                            {'view_layer': None}, 
                            args_super)
        self.set_view(self.make_view())
        
        # Note: Backward compatibility with legacy dialogs that
        #       implement both the view and model layer in a same class
        if self.view_layer != self:
            self.add_owned('view_layer')

        
    def set_view(self, view):
        """set the view layer for this dialog.
        
        **INPUTS**
        
        *ViewLayer view* -- The view layer.
        """
        self.view_layer = view
                    
    def view(self):
        """return the view layer for this dialog.
        
        **OUTPUTS**
        
        *ViewLayer* -- The view layer.
        """
        return self.view_layer
           
    def make_view(self):
       """Factory method for creating a view layer for this dialog.
       
       **OUTPUTS**
       
       *ViewLayer* -- a new view layer for this dialog.
       """
       debug.virtual('DlgModelView.make_view', self)
        
    def Destroy(self):
       """destroy the view layer for this dialog."""
    
       self.view().Destroy()
        
    def ShowModal(self):
        return self.view().ShowModal()
        
    def IsModal(self):
        return self.view().IsModal()
        
    def GetPositionTuple(self):
        return self.view().GetPositionTuple()
        
    def dismiss_event(self):
        """returns a DismissModalEvent which can be used to dismiss the
        dialog

        **INPUTS**

        *none*

        **OUTPUTS**

        *DismissModalEvent* -- the event
        """
        debug.virtual('DlgModelView.dismiss_event', self)


class ViewLayer(Object.OwnerObject):
    """class for implementing view layer of model-view dialogs.

    This uses a simple model-view architecture for dialogs. This
    allows a cleaner separation between business logic and the
    rendering of the dialog, and also, it makes automated unit
    testing easier.
    
    The view is responsible solely for:
      - rendering data graphically upon request from the model
      - intercepting user event and dispatching them to the model
      - note that the view is NOT responsible for coordinating the 
        content of the various fields it displays
      - responding to requests from the model regarding what it is
        currently displaying in a particular field (for assertions in
        unit testing)
      - programmatically simulating user events upon request from the
        model (for unit testing)
      
    The model is responsible for:
      - sending rendering requests to the view
      - responding to requets from the view made in response to
      - coordinating the content of the various fields of the view 
        so that they are always in sync
      - requesting the view to simulate user events (for regression
        testing)

    By convention, any method whose name looks like do_X, is a method
    which is meant to programmatically simulate user action X. Any
    method whose name looks like on_X is a method which is invoked
    when the user does action X.
        
    **INSTANCE ATTRIBUTES**

    *DlgModelView* model -- The model-view dialog that this is the 
    view for.

    **CLASS ATTRIBUTES**
    
    *none* --    
    """
    def __init__(self, model, **args_super):
        self.deep_construct(ViewLayer, 
                            {'the_model': model}, 
                            args_super)
        
    def model(self):
        """return the view layer for this dialog.
        
        **OUTPUTS**
        
        *DlgModelView* -- The model-view dialod.
        """
        return self.the_model
           
                            

# defaults for vim - otherwise ignore
# vim:sw=4
