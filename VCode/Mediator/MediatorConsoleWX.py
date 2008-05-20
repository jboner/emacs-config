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

"""implementation of the MediatorConsole interface for a wxPython GUI mediator
(e.g. wxMediator)

**MODULE VARIABLES**


"""

import debug, messaging
import sys
import string
import shutil
import Object, vc_globals
import MediatorConsole
from wxPython.wx import *
from wxWindowsWithHelpers import *
from thread_communication_WX import *
import exceptions
import os
import threading

font_sz = None
#font_sz=14
font_wt=wxBOLD

class DummyCapture:
    def __init__(self):
        pass

global possible_capture
possible_capture = DummyCapture
try:
    import wxCapture
#    global possible_capture 
    possible_capture = wxCapture.ScreenShotCapture
except exceptions.ImportError:
    pass



# unfortunately, I haven't found any non-MS specific way of manipulating
# the foreground window, nor have I found a way to do it from Python 
# without the win32gui module from the win32all extensions package

import win32gui
import pywintypes


def EVT_MINE(evt_handler, evt_type, func):
    evt_handler.Connect(-1, -1, evt_type, func)

def NO_EVT_MINE(evt_handler, evt_type, func):
    evt_handler.Disconnect(-1, -1, evt_type, func)

# create a unique event types
wxEVT_DISMISS_MODAL = wxNewEventType()

wxID_DISMISS_MODAL = wxNewId()

wxID_CORRECT_NEXT = wxNewId()
wxID_CORRECT_PREV = wxNewId()
wxID_CORRECT_MORE = wxNewId()
wxID_DISCARD_CORRECTION = wxNewId()

def resize_last_column(table):
    """Resize the last column of a table.
    """
    list_client_size = table.GetClientSize()
    n_cols = table.GetColumnCount()
    rest = 0
    for col in range(n_cols -1):
        rest = rest + table.GetColumnWidth(col)
    table.SetColumnWidth(n_cols - 1, list_client_size.width - rest)



def set_text_font(control):
    """set the font size for a control

    **INPUTS**

    *wxWindow control* -- The control (usually a wxTextCtrl, or
    wxListBox) which supports SetFont 

    **OUTPUTS**

    *none*
    """
    if font_sz:
        control.SetFont(wxFont(font_sz, wxMODERN, wxNORMAL, font_wt))


class DismissModalFlagTimerWX(MediatorConsole.DismissModalEvent):
    """implementation of DismissModalEvent using a Python Event flag.  
    The dialog must check this flag periodically

    **INSTANCE ATTRIBUTES**

    *threading.Event bye* -- threading.Event object whose state will be
    set to true if the dialog box should cancel.  

    **CLASS ATTRIBUTES**

    *none*
    """
    def __init__(self, bye, **args):
        self.deep_construct(DismissModalFlagTimerWX,
                            {'bye': bye},
                            args)

    def dismiss(self):
        """send the message, and return synchronously

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        self.bye.set()
        wxWakeUpIdle()
    

class MediatorConsoleWX(MediatorConsole.MediatorConsole):
    """
    **INSTANCE ATTRIBUTES**

    *wxFrame main_frame* -- the main frame window of the console, which
    will be the parent for most modal dialogs

    *(INT, INT) corr_box_pos* -- most recent position of the correction
    box

    *(INT, INT) corr_recent_pos* -- most recent position of the correct
    recent box

    **CLASS ATTRIBUTES**
    
    *none* 
    """
    def __init__(self, main_frame, **attrs):
        self.deep_construct(MediatorConsoleWX,
                            {'main_frame': main_frame,
                             'corr_box_pos': None,
                             'corr_recent_pos': None
                            },
                            attrs, 
                            enforce_value = {'main_frame_handle':
                            main_frame.GetHandle()})

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
        self.main_frame.set_status_text(message)
        
        self.main_frame.log_message(message + "\n")
        return 1

    def message_box(self, message):
        """displays an error or warning message in a message box

        **INPUTS**

        *STR message* -- the message

        **OUTPUTS**

        *none*
        """
        box = wxMessageDialog(self.main_frame, message, "Error", wxOK,
            wxDefaultPosition)
        box.ShowModal()
        box.Destroy()

    def starting_tests(self):
        """method used by NewMediatorObject to notify us that it is
        about to start regression testing

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        self.main_frame.starting_tests()

    def finished_tests(self):
        """method used by NewMediatorObject to notify us that it is
        done with regression testing

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        self.main_frame.finished_tests()


# Hopefully, raise_active_window and raise_wxWindow are made obsolete by
# WinSystemMSW, since the former doesn't work consistently, and the latter
# doesn't seem to work at all under Windows NT (and presumably 2000/XP
# as well)
# However, I'll leave them in until WinSystemMSW has been tested.

    def raise_active_window(self):
        """makes the active window (within the current process) the
        foreground one (for the system)

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        active_handle = win32gui.GetActiveWindow()
        for i in range(2):
            try:
                win32gui.SetForegroundWindow(active_handle)
            except pywintypes.error:
                sys.stderr.write('error restoring window to foreground\n')
            else:
                break

# Hopefully, raise_active_window and raise_wxWindow are made obsolete by
# WinSystemMSW, since the former doesn't work consistently, and the latter
# doesn't seem to work at all under Windows NT (and presumably 2000/XP
# as well)
# However, I'll leave them in until WinSystemMSW has been tested.

    def raise_wxWindow(self, window):
        """makes the given wxWindow the foreground one (for the system)

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        for i in range(2):
            try:
                win32gui.SetForegroundWindow(window.GetHandle())
            except pywintypes.error:
                sys.stderr.write('error restoring window to foreground\n')
            else:
                break

    def show_modal_dialog(self, dialog):
        """shows a dismissable modal dialog box, and handles the push/pop modal 
        bookkeeping
        
        **INPUTS**
        
        *DlgModelView, OwnerObject dialog* -- the dialog to show
        
        **OUTPUTS**
        
        *INT* -- the return code from ShowModal 
        """
        debug.trace('MediatorConsoleWX.show_modal_dialog', '** dialog=%s' % dialog)
        bye = dialog.dismiss_event()
        self.push_modal(bye)
        answer = dialog.ShowModal()
        self.pop_modal()
        return answer

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
        message = """The user_config.py file does not exist.  
One must be created before the mediator can be started.
Select one of the sample user configuration files to use
as an initial user file (which can then be customized).

Optional: if you want to restrict your sessions to less languages, also
copy (manually) the file "user_globals.py" from the samples folder
into to config folder.  See instructions in this file.

              """
        caption = "Missing user configuration file"
        wxMessageBox(message, caption, wxOK, self.main_frame)
        message = "Choose a sample user configuration file"
        wild = "Python scripts (*.py)|*.py"
        dlg = wxFileDialog(self.main_frame, message, directory, "",
            wild, wxOPEN | wxHIDE_READONLY)
        answer = dlg.ShowModal()
        if answer != wxID_OK:
            dlg.Destroy()
            return 0
        path = dlg.GetPath()
        shutil.copy(path, target)
        dlg.Destroy()
        return 1

    def show_correction_box(self, editor_name, utterance, 
        can_reinterpret, should_adapt = 1):
        """display a correction box for correction a complete, recent
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
        original = utterance.words()
        validator = CorrectionValidatorSpoken(utterance = utterance)
        box = CorrectionBoxWX(self, self.main_frame, utterance, validator, 
            can_reinterpret, self.gram_factory, pos = self.corr_box_pos)
#        app = wxGetApp()
#        evt = wxActivateEvent(0, true)
#        app.ProcessEvent(evt)
        answer = self.show_modal_dialog(box)
        self.corr_box_pos = box.GetPositionTuple()
        box.cleanup()
        box.Destroy()
        if answer == wxID_OK:
            if should_adapt:
                utterance.adapt_spoken(utterance.spoken_forms())
            return 1
        else:
            utterance.set_words(original)
            return 0

    def show_recent_utterances(self, editor_name, utterances):
        """display a correct recent dialog box for to allow the user to 
        select a recent utterance to correct

        **INPUTS**

        *STR editor_name* -- name of the editor instance

        *[(SpokenUtterance, INT, BOOL)] utterances* -- the n most recent 
        dictation utterances (or all available if < n), sorted most recent 
        last, each with a corresponding utterance number and a flag 
        indicating if the utterance can be undone and re-interpreted.

        **OUTPUTS**

        *[INT]* -- the utterance numbers of 
        those utterances which were corrected by the user, or None if
        none were corrected
        """
        originals = map(lambda u: u[0].words(), utterances)
        box = CorrectRecentWX(self, self.main_frame, utterances, 
           self.gram_factory, pos = self.corr_recent_pos)

        answer = self.show_modal_dialog(box)
        self.corr_recent_pos = box.GetPositionTuple()
        changed = box.changed()  
        box.cleanup()
        if answer == wxID_OK:
#            print 'answer was OK'
            return changed
        else:
#            print 'answer was cancel'
            return None


    def show_recent_symbols(self, editor_name, symbols):
        """display a dialog box with recent symbols to allow the user to 
        select a recent symbol to reformat

        **INPUTS**

        *STR editor_name* -- name of the editor instance

        *[SymbolResult] symbols* -- symbols interpreted from the n most 
        recent dictation utterances (it is assumed that those utterances
        are reinterpretable).

        **OUTPUTS**

        *[SymbolResult]* -- the list of reformatted symbols.
        """
        debug.trace('MediatorConsoleWX.show_recent_symbols', 'symbols=%s' % repr(symbols))
        box = ReformatRecentSymbols(self, self.main_frame, 
                    symbols, self.gram_factory)
        self.show_modal_dialog(box)
        self.corr_recent_pos = box.GetPositionTuple()
        
        box.cleanup()
#        box.Destroy()
        
        return box.user_reformatted_symbols()

    def destroy_main_frame(self):
        """Destroy the console's main frame"""
        self.main_frame.Destroy()



class DlgModelViewWX(MediatorConsole.DlgModelView):
    """wxPython implementation of a Model-View Dialog.

    **INSTANCE ATTRIBUTES**

    *threading.Event bye* -- threading.Event object whose state will be
    set to true if the dialog box should cancel.  The class
    has a timer event which checks the state of the Event object
    and calls on_dismiss if the Event's state
    becomes true.  The dialog box should then call EndModal with return
    value wxID_DISMISS_MODAL.

    """    
    def __init__(self, **args):
        """
        """
        ID_DISMISS_FLAG_TIMER = wxNewId()
        self.deep_construct(DlgModelViewWX,
                            {
                             'bye': threading.Event(),
                             'the_timer': None
                            }, args,                             
                            exclude_bases = {possible_capture: 1, wxDialog: 1})
        EVT_TIMER(self.view(), ID_DISMISS_FLAG_TIMER, self.check_dismiss_flag)

# this doesn't work because of a bug in wxPython (modal dialog boxes 
# which were started from a custom event handler are created from within
# an idle event, so no other idle events get processed until 
# the modal dialog box closes), so we'll need
# another solution
#        EVT_IDLE(wxGetApp(), self.check_dismiss_flag)
#        EVT_IDLE(self, self.check_dismiss_flag)

        
    def timer(self):
       # Note: timer cannot be created at __init__ time, because
       #       at that point, the view for the dialog may not
       #       have been defined.
       if self.the_timer:
          return the_timer
       else:
          ID_DISMISS_FLAG_TIMER = wxNewId()
          self.the_timer = wxTimer(self.view(), ID_DISMISS_FLAG_TIMER)
          self.the_timer.Start(50)
          return self.the_timer
          
    def delete_timer(self):
       the_timer = None

    def remove_other_references(self):
#        print 'disconnecting event?'
        self.timer().Stop()
        self.delete_timer()
# this doesn't work because of a bug in wxPython (modal dialog boxes 
# which were started from a custom event handler are created from within
# an idle event, so no other idle events get processed until 
# the modal dialog box closes), so we'll need
# another solution
#        wxGetApp().Disconnect(-1, -1, wxEVT_IDLE)
#        print self.Disconnect(-1, -1, wxEVT_IDLE)

    def dismiss_event(self):
        """returns a DismissModalEvent which can be used to dismiss the
        dialog

        **INPUTS**

        *none*

        **OUTPUTS**

        *DismissModalEvent* -- the event
        """
        return DismissModalFlagTimerWX(self.bye)
        

# this doesn't work because of a bug in wxPython (modal dialog boxes 
# which were started from a custom event handler are created from within
# an idle event, so no other idle events get processed until 
# the modal dialog box closes), so we'll need
# another solution
    def check_dismiss_flag_idle(self, event):
#        print 'idle'
        if self.bye.isSet():
            self.on_dismiss()
            return
        event.RequestMore()

    def check_dismiss_flag(self, event):
        if self.bye.isSet():
            self.on_dismiss()
            return

    def on_dismiss(self):
        debug.virtual('DlgModelViewWX.on_dismiss', self)


class CorrectionBoxWX(DlgModelViewWX):
    """dialog for correcting misrecognized dictation results

    **INSTANCE ATTRIBUTES**

    *SpokenUtterance utterance* -- the utterance being corrected
    
    *BOOL first* -- flag indicating whether this is the first time the
    window has been activated.

    *MediatorConsoleWX console* -- the MediatorConsole object which owns
    the correction box

    *ChoiceGram choose_n_gram* -- ChoiceGram supporting "Choose n"

    *ChoiceGram select_n_gram* -- ChoiceGram supporting "SelectOrEdit n"

    *NaturalSpelling spelling_gram* -- NaturalSpelling grammar

    *SimpleSelection selection_gram* -- SimpleSelection grammar for 
    select-and-say in the corrected text control

    *[STR] choices* -- list of alternatives
    """
    def __init__(self, console, parent, utterance, validator, 
            can_reinterpret, gram_factory, pos = None, **args):
        """
        **INPUTS**

        *MediatorConsoleWX console* -- the MediatorConsole object which owns
        the correction box

        *wxWindow parent* -- the parent wxWindow

        *SpokenUtterance utterance* -- the utterance itself

        *CorrectionValidator validator* -- a validator used to transfer
        misrecognized text to the results text field, and corrected text
        back from that field

        *BOOL can_reinterpret* -- flag indicating whether the utterance
        could be reinterpreted upon correction, allowing the correction
        box to give some visual feedback to the user to indictate this.
        Whether the utterance can actually be reinterpreted may change
        between the call to this method and its return, so there is no
        guarantee that reinterpretation will take place.

        *WinGramFactory gram_factory* -- the grammar factory used to add
        speech grammars to the dialog box

        *(INT, INT) pos* -- position of the box in pixels
    
        """
        self.deep_construct(CorrectionBoxWX,
                            {'console': console, 'parent': parent, 'parent': parent, 
                             'utterance': utterance, 'validator': validator, 
                             'can_reinterpret': can_reinterpret,
                             'gram_factory': gram_factory, 'pos': pos},
                            args)
                           
    def make_view(self):
       """factory method for creating the view layer for this dialog"""
       return CorrectionBoxViewWX(self.console, self.parent, self.utterance, 
            self.validator, 
            self.can_reinterpret, self.gram_factory, model = self,
            pos = self.pos)
       

class CorrectionBoxViewWX(MediatorConsole.ViewLayer, wxDialog, possible_capture, Object.OwnerObject):
    """dialog box for correcting misrecognized dictation results

    **INSTANCE ATTRIBUTES**

    *SpokenUtterance utterance* -- the utterance being corrected
    
    *BOOL first* -- flag indicating whether this is the first time the
    window has been activated.

    *MediatorConsoleWX console* -- the MediatorConsole object which owns
    the correction box

    *ChoiceGram choose_n_gram* -- ChoiceGram supporting "Choose n"

    *ChoiceGram select_n_gram* -- ChoiceGram supporting "SelectOrEdit n"

    *NaturalSpelling spelling_gram* -- NaturalSpelling grammar

    *SimpleSelection selection_gram* -- SimpleSelection grammar for 
    select-and-say in the corrected text control

    *[STR] choices* -- list of alternatives
    """
    def __init__(self, console, parent, utterance, validator, 
            can_reinterpret, gram_factory, pos = None, **args):
            
        """
        **INPUTS**

        *MediatorConsoleWX console* -- the MediatorConsole object which owns
        the correction box

        *wxWindow parent* -- the parent wxWindow

        *SpokenUtterance utterance* -- the utterance itself

        *CorrectionValidator validator* -- a validator used to transfer
        misrecognized text to the results text field, and corrected text
        back from that field

        *BOOL can_reinterpret* -- flag indicating whether the utterance
        could be reinterpreted upon correction, allowing the correction
        box to give some visual feedback to the user to indictate this.
        Whether the utterance can actually be reinterpreted may change
        between the call to this method and its return, so there is no
        guarantee that reinterpretation will take place.

        *WinGramFactory gram_factory* -- the grammar factory used to add
        speech grammars to the dialog box

        *(INT, INT) pos* -- position of the box in pixels
    
        """
        possible_capture.__init__(self)
        self.deep_construct(CorrectionBoxViewWX,
                            {
                             'console': console,
                             'parent': parent,
                             'utterance': utterance,
                             'first': 1,
                             'choices': None,
                             'choose_n_gram': None,
                             'select_n_gram': None,
                             'selection_gram': None
                            }, args, 
                            exclude_bases = {possible_capture: 1, wxDialog: 1}
                           )
        if pos is None:
           pos = wxDefaultPosition
        wxDialog.__init__(self, parent, wxNewId(), "Correct an Utterance", pos,
            (600, 500), style = wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER)

        self.name_parent('console')
        self.add_owned('choose_n_gram')
        self.add_owned('select_n_gram')
        self.add_owned('spelling_gram')
        self.add_owned('selection_gram')
        if gram_factory:
            self.choose_n_gram = \
                gram_factory.make_choices(choice_words = ['Choose'])
            self.select_n_gram = \
                gram_factory.make_choices(choice_words = ['Select', 'Edit'])
            self.spelling_gram = \
                gram_factory.make_natural_spelling(spelling_cbk = \
                self.on_spelling)
            self.selection_gram = \
                gram_factory.make_simple_selection(get_visible_cbk = \
                self.get_text, get_selection_cbk = self.get_selection,
                select_cbk = self.on_select_text)
        s = wxBoxSizer(wxVERTICAL)
        intro = wxStaticText(self, wxNewId(), 
            "&Correct the text (use spoken forms)",
            wxDefaultPosition, wxDefaultSize)
        set_text_font(intro)
        init_value = string.join(self.utterance.spoken_forms())
        init_value = ""
# due to a bug in wxWindows, setting the initial value of a text control
# with default size may cause some of the text to be cut off.
#
# instead, we now set the initial value from the validator
        self.text = wxTextCtrl(self, wxNewId(), init_value, wxDefaultPosition,
            (550, 40), style = wxTE_NOHIDESEL, validator = validator)

#        s.Add(self.text, 0, wxEXPAND | wxALL)
        set_text_font(self.text)
        middle_sizer = wxFlexGridSizer(3, 2, 5, 5)
# three rows, two columns, 5 pixels between rows and columns
        number_sizer = wxBoxSizer(wxVERTICAL)
        ID_CHOICES = wxNewId()
        n = 9
        alternatives = self.utterance.alternatives(n)
        spoken_alternatives = []
        for alternative in alternatives:
            spoken_alternatives.append(map(lambda x: x[0], alternative))
        self.choices = map(string.join, spoken_alternatives)
        ID_NUMBERS = wxNewId()
        for i in range(1, 10):
            st = wxStaticText(self, ID_NUMBERS + i, "%d" % i,
                wxDefaultPosition, wxDefaultSize, wxALIGN_RIGHT)
            set_text_font(st)
            number_sizer.Add(st, 0, wxALIGN_RIGHT | wxALIGN_BOTTOM)
        self.choice_list = wxListBox(self, ID_CHOICES, wxDefaultPosition,
             wxDefaultSize, self.choices, wxLB_SINGLE)
        set_text_font(self.choice_list)
        EVT_LISTBOX(self.choice_list, ID_CHOICES, self.on_selected)
        EVT_LISTBOX_DCLICK(self.choice_list, ID_CHOICES, self.on_double)
        bitpath = os.path.join(vc_globals.home, 'Mediator', 'bitmaps')
        yes = wxBitmap(os.path.join(bitpath, 'plus.bmp'), wxBITMAP_TYPE_BMP)
        no = wxBitmap(os.path.join(bitpath, 'minus.bmp'), wxBITMAP_TYPE_BMP)
        if can_reinterpret: 
            which = yes
        else:
            which = no
        maybe = wxStaticBitmap(self, wxNewId(), which,
            wxDefaultPosition, wxDefaultSize)
        middle_sizer.AddMany([(maybe, 0, wxALIGN_CENTER),
                              (intro, 0, wxEXPAND),
                              (0, 0), #spacer
                              (self.text, 0, wxEXPAND | wxALIGN_TOP, 3),
                              (number_sizer, 0, wxEXPAND | wxALIGN_RIGHT),
                              (self.choice_list, 0, wxEXPAND)])
        middle_sizer.AddGrowableRow(2)
#        middle_sizer.AddGrowableRow(1)
        middle_sizer.AddGrowableCol(1)
# not sure why this was needed in the first place - it doesn't seem to
# be now -- DCF
#        if self.choices:
#            self.choice_list.SetSelection(0, 0)
        s.Add(middle_sizer, 1, wxEXPAND | wxALL)
        button_sizer = wxBoxSizer(wxHORIZONTAL)
        ok_button = wxButton(self, wxID_OK, "OK", wxDefaultPosition, 
            wxDefaultSize)
        cancel_button = wxButton(self, wxID_CANCEL, "Cancel", 
            wxDefaultPosition, wxDefaultSize)
        self.playback_button = wxButton(self, wxNewId(), "Playback", 
            wxDefaultPosition, wxDefaultSize)
        if not utterance.playback_available():
            self.playback_button.Enable(0)

        button_sizer.Add(ok_button, 0, wxALL)
        button_sizer.Add(cancel_button, 0, wxALL)
        button_sizer.Add(self.playback_button, 0, wxALL)
        EVT_BUTTON(self, self.playback_button.GetId(), self.on_playback)
        ok_button.SetDefault()
# optionally, add additional buttons
        extra_buttons = self.more_buttons()
        if extra_buttons:
            s.Add(extra_buttons, 0, wxEXPAND | wxALL, 10)
        s.Add(button_sizer, 0, wxEXPAND | wxALL, 10)
#        print 'ids', ok_button.GetId(), wxID_OK
#        win32gui.SetForegroundWindow(self.main_frame.handle)
        EVT_ACTIVATE(self, self.on_activate)
        EVT_CHAR(self.text, self.on_char_text)
        EVT_SET_FOCUS(self.text, self.on_focus_text)
        EVT_KILL_FOCUS(self.text, self.on_kill_focus_text)
        self.Raise()
        self.text.SetFocus()
#        print 'before autolayout: text size is ', self.text.GetSize()
#        print 'before autolayout: choice list size is ', self.choice_list.GetSize()
        self.SetAutoLayout(true)
        self.SetSizer(s)
        self.Layout()
#        print 'before fit: text size is ', self.text.GetSize()
#        print 'before fit: choice list size is ', self.choice_list.GetSize()
        s.Fit(self)
#        print 'after fit: choice list size is ', self.choice_list.GetSize()
#        print 'after fit: text size is ', self.text.GetSize()
#        print 'after fit: text size is ', self.text.GetSize()
#        EVT_MINE(self, wxEVT_DISMISS_MODAL, self.on_dismiss(self))

    def more_buttons(self, button_sizer = None):
        """optionally, add additional buttons

        **INPUTS**

        *wxBoxSizer button_sizer* -- the box sizer for the button row.
        If None and if more_buttons wants to add buttons, it should
        create a new horizontal wxBoxSizer.
      

        **OUTPUTS**

        *wxBoxSizer* -- a reference to the same button sizer,
        containing the added buttons, or None if none was passed to 
        more_buttons and no more buttons were added
        """
        return button_sizer

    def on_dismiss(self):
        self.EndModal(wxID_DISMISS_MODAL)

    def on_playback(self, event):
        ok = self.utterance.playback()
        if not ok:
            self.playback_button.Enable(0)
        self.text.SetFocus()

    def get_text(self):
        return self.text.GetValue()

    def get_selection(self):
        return self.text.GetSelection()

    def on_select_text(self, range):
        if not (range is None):
            self.text.SetSelection(range[0], range[1])

    def on_char_text(self, event):
        k = event.GetKeyCode()
        if k == WXK_UP:
            direction = -1
        elif k == WXK_DOWN:
            direction = 1
        else:
            event.Skip()
            return
        n = self.choice_list.GetSelection()
        n = n + direction
        if n < 0 or n >= len(self.choices):
            return 
        self.choice_list.SetSelection(n)
        self.select_choice(self.choices[n])

    def on_focus_text(self, event):
        if self.spelling_gram:
            self.spelling_gram.activate(self.GetHandle())

    def on_kill_focus_text(self, event):
        if self.spelling_gram:
            self.spelling_gram.deactivate()

    def on_spelling(self, letters):
        """callback called by natural spelling grammar

        **INPUTS**

        *STR* letters -- string of recognized letters

        **OUTPUTS**

        *none*
        """
#        print 'spelled "%s"' % letters
        self.text.WriteText(letters)

    def on_select(self, n):
        """callback called by Select/Edit n grammar to indicate which
        choice was selected

        **INPUTS**

        *INT n* -- the index of the choice selected

        **OUTPUTS**

        *none*
        """
        if n <= self.choice_list.GetCount():
            self.choice_list.SetSelection(n - 1)
            self.select_choice(self.choices[n-1])

    def on_choose(self, n):
        """callback called by Select/Edit n grammar to indicate which
        choice was selected

        **INPUTS**

        *INT n* -- the index of the choice selected

        **OUTPUTS**

        *none*
        """
        if n <= self.choice_list.GetCount():
            self.choice_list.SetSelection(n - 1)
            self.select_choice(self.choices[n-1])
            self.simulate_OK()

    def select_choice(self, text):
        """method which modifies the text field when the selected item
        in the list box changes

        **INPUTS**

        *STR text* -- text of the selected item 

        **OUTPUTS**

        *none*
        """
        self.text.SetValue(text)
        self.text.SetInsertionPointEnd()
        self.text.SetSelection(0, self.text.GetLastPosition())
        self.text.SetFocus()
#        print 'text changing: text size is ', self.text.GetSize()

    def on_selected(self, event):
        self.select_choice(event.GetString())
        event.Skip()

    def simulate_OK(self):
        """method which simulates the user having pressed the Ok button

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        button_event = wxCommandEvent(wxEVT_COMMAND_BUTTON_CLICKED, wxID_OK)
        self.ProcessEvent(button_event)
        
# DCF: For some reason, wxPostEvent doesn't work right if the correction
# box was created from within my custom event handler (though it does if
# it was created from within an EVT_BUTTON handler)
#        wxPostEvent(self, button_event)

    def on_double(self, event):
        self.select_choice(event.GetString())
        self.simulate_OK()

    def on_activate(self, event):
        if self.first:
            if event.GetActive():
                if self.choose_n_gram:
                    self.choose_n_gram.activate(9, self.GetHandle(), 
                        self.on_choose)
                    self.select_n_gram.activate(9, self.GetHandle(), 
                        self.on_select)
                if self.selection_gram:
                    self.selection_gram.activate(window = self.GetHandle())
                self.first = 0
# doesn't work consistently
#                self.console.raise_active_window()
# doesn't work on Win NT
#                self.console.raise_wxWindow(self)
                self.console.win_sys.raise_main_frame()
#                if self.choices:
#                    self.choice_list.SetSelection(0, 0)


class CorrectNextPrevWX(CorrectionBoxWX):
    """subclass of CorrectionBoxWX which adds the option of correcting 
    the next or previous utterance

    **INSTANCE ATTRIBUTES**

    *BOOL first_utterance* -- indicates if this utterance is the first 
    stored utterance, in which case the option to correct the previous 
    utterance should be disabled.

    *BOOL last_utterance* -- indicates if this utterance is the most 
    recent utterance, in which case the option to correct the next 
    utterance should be disabled.
    """
    def __init__(self, first_utterance = 0, last_utterance = 0, **args):
        self.deep_construct(CorrectNextPrevWX, 
                            {'first_utterance': first_utterance,
                             'last_utterance': last_utterance}, 
                             args)

    def more_buttons(self, button_sizer = None):
        """optionally, add additional buttons

        **INPUTS**

        *wxBoxSizer button_sizer* -- the box sizer for the button row.
        If None and if more_buttons wants to add buttons, it should
        create a new horizontal wxBoxSizer.
      

        **OUTPUTS**

        *wxBoxSizer* -- a reference to the same button sizer,
        containing the added buttons, or None if none was passed to 
        more_buttons and no more buttons were added
        """
        button_sizer = CorrectionBoxWX.more_buttons(self, button_sizer)
        correct_previous = wxButton(self, wxNewId(), "Previous Phrase", 
            wxDefaultPosition, wxDefaultSize)
        correct_next = wxButton(self, wxNewId(), "Next Phrase", 
            wxDefaultPosition, wxDefaultSize)
        if self.first_utterance:
            correct_previous.Enable(0)
        if self.last_utterance:
            correct_next.Enable(0)
        if button_sizer is None:
            button_sizer = wxBoxSizer(wxHORIZONTAL)
        button_sizer.Add(correct_previous, 0, wxALL)
        button_sizer.Add(correct_next, 0, wxALL)
        EVT_BUTTON(self, correct_previous.GetId(), 
            self.on_correct_previous)
        EVT_BUTTON(self, correct_next.GetId(), 
            self.on_correct_next)
        return button_sizer

    def on_correct_previous(self, event):
        if self.Validate() and self.TransferDataFromWindow():
            self.EndModal(wxID_CORRECT_PREV)
    def on_correct_next(self, event):
        if self.Validate() and self.TransferDataFromWindow():
            self.EndModal(wxID_CORRECT_NEXT)

class CorrectFromRecentWX(CorrectNextPrevWX):
    """subclass of CorrectNextPrevWX which adds the option of returning to
    the correct recent dialog box for additional corrections, or after
    discarding changes 
    """
    def __init__(self, **args):
        self.deep_construct(CorrectFromRecentWX, {}, args)

    def more_buttons(self, button_sizer = None):
        """optionally, add additional buttons

        **INPUTS**

        *wxBoxSizer button_sizer* -- the box sizer for the button row.
        If None and if more_buttons wants to add buttons, it should
        create a new horizontal wxBoxSizer.
      

        **OUTPUTS**

        *wxBoxSizer* -- a reference to the same button sizer,
        containing the added buttons, or None if none was passed to 
        more_buttons and no more buttons were added
        """
        button_sizer = CorrectNextPrevWX.more_buttons(self, button_sizer)
        more_correction = wxButton(self, wxNewId(), "More Correction", 
            wxDefaultPosition, wxDefaultSize)
        discard_changes = wxButton(self, wxNewId(), "Discard Changes", 
            wxDefaultPosition, wxDefaultSize)
        EVT_BUTTON(self, more_correction.GetId(), 
            self.on_more_correction)
        EVT_BUTTON(self, discard_changes.GetId(), 
            self.on_discard_changes)
        if button_sizer is None:
            button_sizer = wxBoxSizer(wxHORIZONTAL)
        button_sizer.Add(more_correction, 0, wxALL)
        button_sizer.Add(discard_changes, 0, wxALL)
        return button_sizer

    def on_more_correction(self, event):
        if self.Validate() and self.TransferDataFromWindow():
            self.EndModal(wxID_CORRECT_MORE)

    def on_discard_changes(self, event):
        self.EndModal(wxID_DISCARD_CORRECTION)



class CorrectionValidator(wxPyValidator, Object.Object):
    """abstract base class for classes used to transfer (mis-)recognized text 
    to the correction box, and retreive corrected text from the 
    correction box when the ok button is pressed"""
    def __init__(self, utterance, **args):
        """
        **INPUTS**

        *SpokenUtterance utterance* -- the utterance to which the
        correction will be made
        """
        self.deep_construct(CorrectionValidator,
                            {
                             'utterance': utterance
                            }, args, exclude_bases = {wxPyValidator:1})
        wxPyValidator.__init__(self)

    def args_for_clone(self, dict):
        """adds keyword arguments needed to clone this class to a
        dictionary

        **NOTE:** subclasses which override this method must call their 
        parent class's args_for_clone method.
        
        **INPUTS**

        *{STR:ANY} dict* -- dictionary of keyword arguments

        **OUTPUTS**

        *none*

        **NOTE:** while this method has no outputs, it must modify the
        input dictionary by adding to it any constructor arguments needed to 
        clone the object.  
        """
        dict['utterance'] = self.utterance

    def Clone(self):
        """copies the validator.
        
        subclass must override this to create a
        copy of the appropriate subclass

        **INPUTS**

        *none*

        **OUTPUTS**

        *same as self* -- a copy of the validator (not a reference to
        self)
        """
        debug.virtual('CorrectionValidator.Clone')

    def Validate(self, win):
        """validate the contents of the text field

        **INPUTS**

        *wxWindow win* -- wxTextCtrl representing the corrected text

        **OUTPUTS**

        *BOOL* -- true if the field contents are valid
        """
        return true

    def initial_value(self):
        """retrieve the initial value for the (mis-)recognized text
        from the  utterance

        **INPUTS**

        *none*

        **OUTPUTS**

        *STR* -- the recognized text as a string
        """
        debug.virtual('CorrectionValidator.initial_value')

    def convert_corrected(self, corrected):
        """accepts the converts the corrected text as a string 
        and corrects the utterance (but does not adapt)

        **INPUTS**

        *STR corrected* -- the corrected text

        **OUTPUTS**

        *BOOL* -- true if the corrected text was successfully converted 
        (otherwise, the dialog box won't close)
        """
        debug.virtual('CorrectionValidator.convert_corrected')

    def TransferToWindow(self):
        """Transfer data from validator to window.

        **INPUTS**

        *none*

        **OUTPUTS**

        *BOOL* -- true if data was successfully transfered to the text
        control
        """
        win = self.GetWindow()
        text = self.initial_value()
        win.SetValue(text)
        win.SetInsertionPointEnd()
        win.SetSelection(0, len(text))
#        print text
#        print win.GetValue()
#        print win.GetSelection()
#        print win.GetSize()
#        win.Refresh()
#        print win.GetSize()
#        parent = win.GetParent()
#        parent.Raise()
#        print parent.parent.handle
#        for i in range(5):
#            w = win32gui.GetWindow(parent.parent.handle, 0)
#            print w
#            print win32gui.GetWindowText(w)
#        hf = win32gui.GetForegroundWindow()
#        ha = win32gui.GetActiveWindow()
#        print 'foreground, active = ', hf, ha
#        print 'dialog shown: ', parent.IsShown()
#        win32gui.SetForegroundWindow(ha)
        return true


    def TransferFromWindow(self):
        """Transfer data from window to validator.

        **INPUTS**

        *none*

        **OUTPUTS**

        *BOOL* -- true if data was successfully transfered from the text
        control
        """
        win = self.GetWindow()
        parent = win.GetParent()
#        parent.parent = None
#        print 'transferring from window'
        corrected_spoken = win.GetValue()
        corrected_spoken.encode("ascii", "ignore")
        valid = self.convert_corrected(corrected_spoken)
        return valid

class CorrectionValidatorSpoken(CorrectionValidator):
    """simplest possible implementaion of CorrectionValidator,
    assuming that the corrected text is a space-delimited list of spoken
    forms.
    """
    def __init__(self, **args):
        self.deep_construct(CorrectionValidatorSpoken,
                            {
                            }, args)

    def args_for_clone(self, dict):
        """adds keyword arguments needed to clone this class to a
        dictionary

        **NOTE:** subclasses which override this method must call their 
        parent class's args_for_clone method.
        
        **INPUTS**

        *{STR:ANY} dict* -- dictionary of keyword arguments

        **OUTPUTS**

        *none*

        **NOTE:** while this method has no outputs, it must modify the
        input dictionary by adding to it any constructor arguments needed to 
        clone the object.  
        """
        CorrectionValidator.args_for_clone(self, dict)

    def Clone(self):
        """copies the validator.
        
        subclass must override this to create a
        copy of the appropriate subclass

        **INPUTS**

        *none*

        **OUTPUTS**

        *same as self* -- a copy of the validator (not a reference to
        self)
        """
        dict = {}
        self.args_for_clone(dict)
        return apply(CorrectionValidatorSpoken, [], dict)

    def initial_value(self):
        """retrieve the initial value for the (mis-)recognized text
        from the  utterance

        **INPUTS**

        *none*

        **OUTPUTS**

        *STR* -- the recognized text as a string
        """
        return string.join(self.utterance.spoken_forms())

    def convert_corrected(self, corrected):
        """accepts the converts the corrected text as a string 
        and corrects the utterance (but does not adapt)

        **INPUTS**

        *STR corrected* -- the corrected text

        **OUTPUTS**

        *BOOL* -- true if the corrected text was successfully converted 
        (otherwise, the dialog box won't close)
        """
        self.utterance.set_spoken(string.split(corrected))
        return 1

class CorrectRecentWX(DlgModelViewWX):
    """dialog box which lists recently dictated utterances, allowing the user 
    to select one for correction of misrecognized results or for symbol 
    reformatting

    **INSTANCE ATTRIBUTES**

    *[(SpokenUtterance, INT, BOOL)] utterances* -- the n most recent 
    dictation utterances (or all available if < n), sorted most recent 
    last, each with a corresponding utterance number and a flag 
    indicating if the utterance can be undone and re-interpreted.

    *BOOL first* -- flag indicating whether this is the first time the
    window has been activated.

    *MediatorConsoleWX console* -- the MediatorConsole object which owns
    the correction box

    *WinGramFactory gram_factory* -- the grammar factory used to add
    speech grammars to the dialog box

    *ChoiceGram correct_n_gram* -- ChoiceGram supporting "Correct n"
    """
    def __init__(self, console, parent, utterances, 
            gram_factory, pos=None, **args):
        """
        **INPUTS**

        *MediatorConsoleWX console* -- the MediatorConsole object which owns
        the correction box

        *[(SpokenUtterance, INT, BOOL)] utterances* -- the n most recent 
        dictation utterances (or all available if < n), sorted most 
        recent last, with corresponding flags indicating if the utterance 
        can be undone and re-interpreted

        *{INT: BOOL} corrected* -- set of utterances which have been
        corrected, counted from most recent = 1

        *WinGramFactory gram_factory* -- the grammar factory used to add
        speech grammars to the dialog box
        """
        self.deep_construct(CorrectRecentWX,
                            {
                             'console': console,
                             'parent': parent, 
                             'utterances': utterances,
                             'gram_factory': gram_factory,
                             'pos': pos,
                            }, args, 
                            exclude_bases = {}
                           )
                           
    def make_view(self):
        return CorrectRecentViewWX(self.console, self.parent, self.utterances, 
                                       self.gram_factory, model = self,
                                       pos = self.pos)
                                       
    def changed(self):
        return self.view().changed()


class CorrectRecentViewWX(MediatorConsole.ViewLayer, wxDialog, possible_capture, Object.OwnerObject):
    """view layer for dialog box which lists recently dictated utterances, allowing the user 
    to select one for correction of misrecognized results or for symbol 
    reformatting

    **INSTANCE ATTRIBUTES**

    *[(SpokenUtterance, INT, BOOL)] utterances* -- the n most recent 
    dictation utterances (or all available if < n), sorted most recent 
    last, each with a corresponding utterance number and a flag 
    indicating if the utterance can be undone and re-interpreted.

    *BOOL first* -- flag indicating whether this is the first time the
    window has been activated.

    *MediatorConsoleWX console* -- the MediatorConsole object which owns
    the correction box

    *WinGramFactory gram_factory* -- the grammar factory used to add
    speech grammars to the dialog box

    *ChoiceGram correct_n_gram* -- ChoiceGram supporting "Correct n"
    """
    def __init__(self, console, parent, utterances, 
            gram_factory, pos = None, **args):
        """
        **INPUTS**

        *MediatorConsoleWX console* -- the MediatorConsole object which owns
        the correction box

        *[(SpokenUtterance, INT, BOOL)] utterances* -- the n most recent 
        dictation utterances (or all available if < n), sorted most 
        recent last, with corresponding flags indicating if the utterance 
        can be undone and re-interpreted

        *{INT: BOOL} corrected* -- set of utterances which have been
        corrected, counted from most recent = 1

        *WinGramFactory gram_factory* -- the grammar factory used to add
        speech grammars to the dialog box
        """
        possible_capture.__init__(self)
        self.deep_construct(CorrectRecentViewWX,
                            {
                             'console': console,
                             'utterances': utterances,
                             'gram_factory': gram_factory,
                             'parent': parent,
                             'first': 1,
                             'nth_event': CorrectNthEventWX(self),
                             'corrected': {},
                             'correct_n_gram': None,
                            }, args, 
                            exclude_bases = {possible_capture:1, wxDialog:1}
                           )
        if pos is None:
           pos = wxDefaultPosition 
        wxDialog.__init__(self, parent, wxNewId(), "Correct Recent Utterances", pos,
           (600, 500), style = wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER)
        
        self.name_parent('console')
        self.add_owned('correct_n_gram')
        if gram_factory:
            if wxMAJOR_VERSION > 2 or \
                (wxMAJOR_VERSION == 2 and 
                     (wxMINOR_VERSION > 3 or 
                          (wxMINOR_VERSION == 3 and wxRELEASE_NUMBER >= 4)
                     )
                ):
                self.correct_n_gram = \
                    gram_factory.make_choices(choice_words = ['Correct'])

        s = wxBoxSizer(wxVERTICAL)
        intro = wxStaticText(self, wxNewId(), 
            "&Choose a phrase to correct",
            wxDefaultPosition, wxDefaultSize)
        set_text_font(intro)
        s.Add(intro, 0, wxEXPAND | wxALL)
        recent = wxListCtrlWithHelpers(self, wxNewId(), wxDefaultPosition,
            wxDefaultSize, 
            style = wxLC_REPORT | wxLC_HRULES | wxLC_SINGLE_SEL)
        set_text_font(recent)
        recent.InsertColumn(0, "#")
        recent.InsertColumn(1, "Spoken phrase")
        phrases = map(lambda x: string.join(x[0].spoken_forms()),
            utterances)
        can_reinterpret = map(lambda x: x[2], utterances)
        index = range(len(phrases), 0, -1)
        bitpath = os.path.join(vc_globals.home, 'Mediator', 'bitmaps')
        yes = wxBitmap(os.path.join(bitpath, 'small_plus.bmp'), wxBITMAP_TYPE_BMP)
        no = wxBitmap(os.path.join(bitpath, 'small_minus.bmp'), wxBITMAP_TYPE_BMP)
        self.images = wxImageList(16, 16)
        index_no = self.images.Add(no)
        index_yes = self.images.Add(yes)
# I'm guessing that LC_REPORT uses small images
        recent.SetImageList(self.images, wxIMAGE_LIST_SMALL)
        for i in range(len(phrases)):
            if can_reinterpret[i]: 
                which = index_yes
            else:
                which = index_no
            recent.InsertImageStringItem(i, str(index[i]), which)
            recent.SetStringItem(i, 1, phrases[i])
        recent.SetColumnWidth(0, wxLIST_AUTOSIZE)
        recent.SetColumnWidth(1, wxLIST_AUTOSIZE)

        recent.ScrollList(0, len(phrases))
        self.recent = recent
        self.phrases = phrases
        s.Add(recent, 1, wxEXPAND | wxALL)
        okb = wxButtonWithHelpers(self, wxID_OK, "OK", wxDefaultPosition, wxDefaultSize)
        cancelb = wxButtonWithHelpers(self, wxID_CANCEL, "Cancel", wxDefaultPosition, wxDefaultSize)
#        EVT_BUTTON(self, okb.GetId(), self.on_ok)
        b_sizer = wxBoxSizer(wxHORIZONTAL)
        b_sizer.Add(okb, 0, 0)
        b_sizer.Add(cancelb, 0, 0)
        s.Add(b_sizer, 0, wxEXPAND | wxALL)
        
        EVT_ACTIVATE(self, self.on_activate)
        EVT_CHAR(self, self.on_char)
        EVT_LIST_ITEM_ACTIVATED(self.recent, self.recent.GetId(), self.on_choose)        
        self.SetAutoLayout(true)
        self.SetSizer(s)
        self.Layout()
        actual = s.GetSize()
        minimum = s.GetMinSize()
        list_size = self.recent.GetSize()
        list_client_size = self.recent.GetClientSize()
        h = list_client_size.GetHeight()
        w = list_client_size.GetWidth()
        s.SetItemMinSize(self.recent, w, h)
        s.SetMinSize(wxSize(0, actual.GetHeight()))
        q = s.GetMinSize()
        s.Fit(self)
        s.SetMinSize(wxSize(q.GetWidth(), 0))
        self.resize_last_column()
        last = len(self.phrases)-1
        self.recent.EnsureVisible(last)
        self.recent.SetItemState(last, wxLIST_STATE_SELECTED, wxLIST_STATE_SELECTED)
        self.recent.SetItemState(last, wxLIST_STATE_FOCUSED, wxLIST_STATE_FOCUSED)
        self.hook_events()
        
    def hook_events(self):
        """hook events up to our handlers

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        EVT_MINE(self, wxEVT_CORRECT_NTH_RECENT, self.on_nth_by_voice)

    def resize_last_column(self):
        list_client_size = self.recent.GetClientSize()
        n_cols = self.recent.GetColumnCount()
        rest = 0
        for col in range(n_cols -1):
            rest = rest + self.recent.GetColumnWidth(col)
        self.recent.SetColumnWidth(n_cols - 1, list_client_size.width - rest)

    def focus_recent(self):
        self.recent.SetFocus()

    def on_choose(self, event):
        i = event.GetIndex()
#        print 'on_choose, %d' %i
        self.chose_from_list(i)

    def correct_nth(self, n):
        """display a correction box for correction a complete, recent
        utterance, accept user corrections, and allow the user to
        approve or cancel.

        **INPUTS**

        *INT n* -- correct nth most recent utterance

        **OUTPUTS**

        *STR* -- string indicating the outcome of the correction dialog:
        allowed values are 'ok', 'cancel', 'next', 'previous', 'more',
        'discard'
        """
        u = self.utterances[-n]
        utterance = u[0]
        can_reinterpret = u[2]
        validator = CorrectionValidatorSpoken(utterance = utterance)
        box = CorrectFromRecentWX(console = self.console, parent = self, 
            utterance = utterance, validator = validator, 
            can_reinterpret = can_reinterpret, gram_factory = self.gram_factory,
            pos = self.console.corr_box_pos,
            last_utterance = (n == 1), 
            first_utterance = (n == len(self.utterances)))
        answer = self.console.show_modal_dialog(box)
        self.console.corr_box_pos = box.GetPositionTuple()

        box.cleanup()

        if answer == wxID_OK:
            return 'ok'
        elif answer == wxID_CANCEL:
            return 'cancel'
        elif answer == wxID_CORRECT_NEXT:
            return 'next'
        elif answer == wxID_CORRECT_PREV:
            return 'previous'
        elif answer == wxID_CORRECT_MORE:
            return 'more'
        elif answer == wxID_DISCARD_CORRECTION:
            return 'discard'
# shouldn't happen, but ...
        return None

    def chose_from_list(self, i):
        n = len(self.phrases) - i
        original_words = self.utterances[-n][0].words()
        answer = self.correct_nth(n)
        if answer == 'cancel':
            self.EndModal(wxID_CANCEL)
            return
        if answer == 'discard':
            self.utterances[-n][0].set_words(original_words)
            self.focus_recent()
            return
        self.corrected[n] = 1
        self.phrases[i] = string.join(self.utterances[i][0].spoken_forms())
        self.recent.SetStringItem(i, 1, self.phrases[i])
        self.resize_last_column()
        if answer == 'next':
            self.chose_from_list(i + 1)
        elif answer == 'previous':
            self.chose_from_list(i - 1)
        elif answer == 'ok':
            if self.Validate() and self.TransferDataFromWindow():
                self.EndModal(wxID_OK)
        self.focus_recent()
        return
#        str = 'You chose item %d: "%s"' % (n, self.phrases[-n])
#        m = wxMessageDialog(self, str, style = wxOK)
#        m.ShowModal()
#        m.Destroy()

#    def on_ok(self, event):
#        print 'hit ok'
#        event.Skip()

    def on_char(self, event):
        k = event.GetKeyCode()
        if k == WXK_PRIOR:
            top = self.recent.GetTopItem()
            page = self.recent.GetCountPerPage()
            new_top = top-page
            if new_top < 0:
                new_top = 0
            self.recent.EnsureVisible(new_top)
        elif k == WXK_NEXT:
            top = self.recent.GetTopItem()
            page = self.recent.GetCountPerPage()
            last = self.recent.GetItemCount()
            new_bottom = top+ 2*page
            if new_bottom >= last:
                new_bottom = last -1
            self.recent.EnsureVisible(new_bottom)
        else:
            event.Skip()
            return

    def on_activate(self, event):
        if self.first:
#            print 'first'
            if event.GetActive():
                if self.correct_n_gram:
                    self.correct_n_gram.activate(len(self.phrases), 
                        self.GetHandle(), self.chose_by_voice)
                self.first = 0
                self.console.raise_wxWindow(self)
                self.focus_recent()

    def chose_by_voice(self, n):
        i = len(self.phrases) - n
# send an event to notify self to bring up correct n asynchronously
        self.nth_event.notify(i)

    def on_nth_by_voice(self, event):
# bring up correct n synchronously
        self.chose_from_list(event.recent_chosen)

    def changed(self):
        """reports which utterances have been corrected

        **INPUTS**

        *none*
        
        **OUTPUTS**

        *[INT]* -- the indices of those utterances which have been 
        corrected by the user, counted from most recent = 1, or 
        None if none were corrected
        """
        return self.corrected.keys()


class ReformatRecentSymbols(DlgModelViewWX):
    """MODEL for dialog box which lists recently dictated symbols, allowing the user 
    to select one for reformatting

    **INSTANCE ATTRIBUTES**

    *[SymbolResults] symbols* -- the n most recently uttered symbols
    sorted most recent last.

    *BOOL first* -- flag indicating whether this is the first time the
    dialog has been activated.

    *MediatorConsoleWX console* -- the MediatorConsole object which owns
    the correction box
    
    *wxDialog parent* -- The parent window for this dialog.

    *WinGramFactory gram_factory* -- the grammar factory used to add
    speech grammars to the dialog box
    
    *INT pos* -- position of the symbol to reformat.
    
    *ReformatFromRecentWX dlg_reformat_from_recent = None* -- dialog to be used 
    to reformat a selected symbol.
    """

    def __init__(self, console, parent, symbols, 
                 gram_factory, pos = None, dlg_reformat_from_recent=None,
                 **args):                                      
       self.deep_construct(ReformatRecentSymbols, 
                           {'symbols': symbols,
                            'console': console,
                            'gram_factory': gram_factory,
                            'pos': pos,
                            'parent': parent,
                            'dlg_reformat_from_recent': dlg_reformat_from_recent
                           },
                           args)
       self.add_owned('dlg_reformat_from_recent')

    def make_view(self):
       return ReformatRecentSymbolsViewWX(self.console, self.parent,
                                          self.symbols, 
                                          self.gram_factory, self.pos,
                                          model = self)

    def displayed_symbols(self):
        return self.view().displayed_symbols()

    def Show(self, flag=None):
        if flag == None:
            return self.view().Show()
        else:
            return self.view().Show(flag)
            
    def on_activate(self, event):
        return self.view().on_activate(event)
        
    def do_choose(self, nth):
        return self.view().do_choose(nth)
                          
    def do_cancel(self):
        return self.view().do_cancel()
        
    def do_ok(self):
        return self.view().do_ok()
        
    def on_ok(self, event=None):
        debug.trace('ReformatRecentSymbols.on_ok', '** invoked')
        pass
      
    def on_cancel(self, event=None):
        self.void_all_user_reformattings()
       
    def void_all_user_reformattings(self):
        for a_symbol in self.symbols:
           a_symbol.reformat_to(None)
       
    def selected_symbol_index(self):
       """returns index (in the list of displayed symbols) of the currently selected
       symbol."""
       return self.view().selected_symbol_index()

    def reformat_nth(self, nth):
        symbol = self.symbols[nth]
        box = self.make_reformat_from_recent_dlg(symbol)
        self.console.show_modal_dialog(box)
        if box.was_okayed:
           self.do_ok()
        else:
           self.do_cancel()
           
        
    def user_reformatted_symbols(self):
       """returns the list of symbols which were reformatted
       by the user
       
       **OUTPUTS**
       
       *[SymbolResult]* -- the list of symbols.
       """
       reformatted_symbols = []
       for a_symbol in self.symbols:
          if a_symbol.reformatted_to:
             reformatted_symbols.append(a_symbol)
       return reformatted_symbols
       
        
    def make_reformat_from_recent_dlg(self, symbol):
       """creates a dialog for reformattting
       a single symbol.
     
       **INPUTS**
        
       *SymbolResult symbol* -- the symbol to reformat.
      
       **OUTPUTS**
       
       *ReformatFromRecentWX* -- the dialog
       """
       if not self.dlg_reformat_from_recent:
          self.dlg_reformat_from_recent = \
              ReformatFromRecentWX(console = self.console, parent = self.view(), 
                                   symbol = symbol)
       self.dlg_reformat_from_recent.reset(symbol)

          
       return self.dlg_reformat_from_recent
       
       

class ReformatRecentSymbolsViewWX(MediatorConsole.ViewLayer, wxDialogWithHelpers, possible_capture, 
                              Object.OwnerObject):
    """dialog box which lists recently dictated symbols, allowing the user 
    to select one for reformatting

    **INSTANCE ATTRIBUTES**

    *[SymbolResults] symbols* -- A list of symbols that the user could
    reformat. It is assumed that all of those symbols CAN be reformatted
    (i.e. that we can reinterpret the utterances where these symbols 
    were spoken).

    *BOOL first* -- flag indicating whether this is the first time the
    window has been activated.

    *MediatorConsoleWX console* -- the MediatorConsole object which owns
    the correction box
    
    *wxDialog parent* -- the parent window.

    *WinGramFactory gram_factory* -- the grammar factory used to add
    speech grammars to the dialog box

    *ChoiceGram correct_n_gram* -- ChoiceGram supporting "Correct n"
    """
    def __init__(self, console, parent, symbols, 
            gram_factory, pos = None, **args):
            
        """
        **INPUTS**

        *MediatorConsoleWX console* -- the MediatorConsole object which owns
        the correction box

        *[SymbolResults] symbols* -- A list of symbols that the user could
        reformat. It is assumed that all of those symbols CAN be reformatted
        (i.e. that we can reinterpret the utterances where these symbols 
        were spoken).

        *WinGramFactory gram_factory* -- the grammar factory used to add
        speech grammars to the dialog box

        *(INT, INT) pos* -- position of the box in pixels
        """
        use_pos = pos
        if pos is None:
            use_pos = wxDefaultPosition
        wxDialogWithHelpers.__init__(self, parent, wxNewId(), "Reformat Recent Symbols", use_pos,
            (600, 400),
            style = wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER)
           
        possible_capture.__init__(self)
        self.deep_construct(ReformatRecentSymbolsViewWX,
                            {
                             'console': console,
                             'symbols': symbols,
                             'gram_factory': gram_factory,
                             'first': 1,
                             'nth_event': CorrectNthEventWX(self),
                             'corrected': {},
                             'correct_n_gram': None,
                            }, args, 
                            exclude_bases = {possible_capture:1, wxDialogWithHelpers: 1}
                           )
                           
        
        self.name_parent('console')
        self.add_owned('correct_n_gram')
        if gram_factory:
            if wxMAJOR_VERSION > 2 or \
                (wxMAJOR_VERSION == 2 and 
                     (wxMINOR_VERSION > 3 or 
                          (wxMINOR_VERSION == 3 and wxRELEASE_NUMBER >= 4)
                     )
                ):
                self.correct_n_gram = \
                    gram_factory.make_choices(choice_words = ['Correct'])
        if pos is None:
            self.CenterOnScreen()

        main_sizer = wxBoxSizer(wxVERTICAL)
        intro = wxStaticText(self, wxNewId(), 
            "&Choose a symbol to correct",
            wxDefaultPosition, wxDefaultSize)
        set_text_font(intro)
        main_sizer.Add(intro, 0, wxEXPAND | wxALL)
        
        recent = wxListCtrlWithHelpers(self, wxNewId(), wxDefaultPosition,
            wxDefaultSize, 
            style = wxLC_REPORT | wxLC_HRULES | wxLC_SINGLE_SEL)
        set_text_font(recent)
        
        recent.InsertColumn(0, "")
        recent.InsertColumn(1, "Written symbol")
        recent.InsertColumn(2, "Spoken symbol") 
        recent.InsertColumn(3, "In utterance") 
                                              
        phrases = map(lambda x: x.in_utter_interp.phrase_as_string(),
                      symbols)
        index = range(len(phrases), 0, -1)            
          
        for ii in range(len(symbols)):
           recent.InsertStringItem(ii, str(index[ii]))
           recent.SetStringItem(ii, 1, self.symbols[ii].native_symbol())
           recent.SetStringItem(ii, 2, string.join(self.symbols[ii].spoken_phrase()))
           recent.SetStringItem(ii, 3, 
                                 self.symbols[ii].in_utter_interp.phrase_as_string())

        recent.SetColumnWidth(0, wxLIST_AUTOSIZE_USEHEADER)
        recent.SetColumnWidth(1, wxLIST_AUTOSIZE_USEHEADER)
        recent.SetColumnWidth(2, wxLIST_AUTOSIZE_USEHEADER)
        recent.SetColumnWidth(3, wxLIST_AUTOSIZE_USEHEADER)

        recent.ScrollList(0, len(symbols))
        self.recent = recent
        
        
        main_sizer.Add(recent, 1, wxEXPAND | wxALL)
        self.okb = wxButtonWithHelpers(self, wxID_OK, "OK", wxDefaultPosition, wxDefaultSize)
        self.cancelb = wxButtonWithHelpers(self, wxID_CANCEL, "Cancel", wxDefaultPosition, wxDefaultSize)              
        EVT_BUTTON(self, self.okb.GetId(), self.on_ok)
        EVT_BUTTON(self, self.cancelb.GetId(), self.on_cancel)        
        b_sizer = wxBoxSizer(wxHORIZONTAL)
        b_sizer.Add(self.okb, 0, 0)
        b_sizer.Add(self.cancelb, 0, 0)
        main_sizer.Add(b_sizer, 0, wxEXPAND | wxALL)

# note: neither of these handlers gets called if a child control 
# has the focus.
# I thought they would be called if the focused control didn't have a
# handler
        EVT_ACTIVATE(self, self.on_activate)
        EVT_CHAR(self.recent, self.on_recent_char)
        EVT_LIST_ITEM_SELECTED(self.recent, self.recent.GetId(), self.on_select)
        EVT_LIST_ITEM_ACTIVATED(self.recent, self.recent.GetId(), self.on_choose)

        self.SetAutoLayout(true)
        self.SetSizer(main_sizer)
        self.Layout()
        actual = main_sizer.GetSize()
        minimum = main_sizer.GetMinSize()
        list_size = self.recent.GetSize()
        list_client_size = self.recent.GetClientSize()
        h = list_client_size.GetHeight()
        w = list_client_size.GetWidth()
        main_sizer.SetItemMinSize(self.recent, w, h)
        main_sizer.SetMinSize(wxSize(0, actual.GetHeight()))
        q = main_sizer.GetMinSize()
        main_sizer.Fit(self)
        main_sizer.SetMinSize(wxSize(q.GetWidth(), 0))
        resize_last_column(self.recent)
        last = len(self.symbols)-1
        self.recent.EnsureVisible(last)
        self.recent.SetItemState(last, wxLIST_STATE_SELECTED, wxLIST_STATE_SELECTED)
        self.recent.SetItemState(last, wxLIST_STATE_FOCUSED, wxLIST_STATE_FOCUSED)
        
        
    def on_ok(self, event=None):
        self.model().on_ok(event)
        self.EndModal(wxID_OK)
        
    def on_cancel(self, event=None):
        self.model().on_cancel(event)
        debug.trace('ReformatRecentSymbolsViewWX.on_cancel', '** before EndModal, self=%s' % self)
        self.EndModal(wxID_CANCEL)
        debug.trace('ReformatRecentSymbolsViewWX.on_cancel', '** after EndModal')

    def on_activate(self, event):
        pass

    def on_char(self, event):
        debug.not_implemented('ReformatRecentSymbolsViewWX.on_char')

    def on_recent_char(self, event):
        keycode = event.GetKeyCode()
        if self.recent.HandleUpOrDownArrow(keycode):
           return
        event.Skip()

    def on_choose_selected_symbol(self):
        """invoked when symbol currently selected in the symbols list
        is chosen by the user for correction"""
        self.model().reformat_nth(self.recent.GetFirstSelected())        

    def on_choose(self, event):
        """invoked when a symbol is chosen in the symbol list.

        **INPUTS**

        *ListSelectionEvent event* -- list selection event.

        **OUTPUTS**

        - None -
        """
        debug.trace('ReformatRecentSymbolsView.on_choose', 'invoked')
        self.model().reformat_nth(event.GetIndex())
        
    def displayed_symbols(self):
       return self.recent.AllCellsContentsString()

    def on_activate(self, event):
       self.console.win_sys.raise_main_frame()
       
    def on_select(self, event):
       debug.trace('ReformatRecentSymbolsView.on_select', 'invoked')
       pass
       
    def do_choose(self, nth):
       self.recent.Select(nth)
       return self.on_choose_selected_symbol()
       
    def selected_symbol_index(self):
       """returns index (in the list of displayed symbols) of the currently selected
       symbol."""
       return self.recent.GetNextSelected(-1)

    def do_cancel(self):
       self.cancelb.Click()
       
    def do_ok(self):
       self.okb.Click()
       
class ReformatFromRecentWX(DlgModelViewWX):
    """model-view dialog for reformatting one dictated symbol.

    **INSTANCE ATTRIBUTES**

    *SymbolResults symbol* -- the symbol to be corrected.
    
    *BOOL was_okayed=false* -- true IIF the dialog was OKayed as opposed to 
    cancelled.

    *MediatorConsoleWX console* -- the MediatorConsole object which owns
    the correction box
    
    *wxDialog parent* -- The parent window for this dialog.
    """

    def __init__(self, console, parent, symbol, **args):                                      
       self.deep_construct(ReformatFromRecentWX, 
                           {'symbol': symbol,
                            'was_okayed': false,
                            'console': console,
                            'parent': parent
                           },
                           args)

    def make_view(self):
       debug.trace('ReformatFromRecentWX.make_view', 'self.parent=%s' % self.parent)
       return ReformatFromRecentViewWX(self.console, self.parent, model = self)
                                          
    def reset(self, symbol):
       """reinitialise the dialog
       
       **INPUTS**
       
       SymbolResult symbol -- The symbol to reinitialise with."""
       
       self.symbol = symbol
       self.view().reset(symbol)
       
    def intro(self):
       return self.view().intro()
       
    def chosen_form(self):
       """returns the written form of the symbol being displayed by the 
       view layer"""
       return self.view().chosen_form()
       
    def displayed_list_of_alternate_forms(self):
       """returns the list of alternate forms for the symbol that are 
       displayed by the view"""
       return self.view().displayed_list_of_alternate_forms()

    def do_select_nth_form(self, nth):
       """click on the nth alternate form for the symbol"""
       self.view().do_select_nth_form(nth)
       
    def on_select_alternate_form(self, nth):
       form = self.symbol.suggestions_list()[nth]
       self.view().set_alternate_form(form)

    def do_choose_nth_form(self, nth):
       self.view().do_choose_nth_form(nth)
              
    def on_choose_alternate_form(self, nth):
       self.on_select_alternate_form(nth)
       self.do_ok()
       
    def do_cancel(self):
       self.view().do_cancel()
       
    def on_cancel(self, event=None):
       self.was_okayed = false
       self.symbol.reformat_to(None)         

    def on_ok(self, event=None):
       self.was_okayed = true
       self.symbol.reformat_to(self.chosen_form())
       
    def do_ok(self):
       self.view().do_ok()
       
    def do_type_form(self, form):
       self.view().do_type_form(form)

    def on_format_pick_list_char(self, event):
        key = event.GetKeyCode()
        if key == WXK_UP:
            direction = -1
        elif key == WXK_DOWN:
            direction = 1
        else:
            event.Skip()
            return
        format_index = self.view().selected_alternate_form_index()
        format_index = format_index + direction
        if format_index >= 0 and format_index < len(self.displayed_list_of_alternate_forms()):
           self.do_select_nth_form(format_index)
          
    def selected_alternate_form_index(self):
       return self.view().selected_alternate_form_index()

       
class ReformatFromRecentViewWX(MediatorConsole.ViewLayer, wxDialogWithHelpers, possible_capture, 
                              Object.ChildObject):
    """dialog box for reformatting a single symbol.

    **INSTANCE ATTRIBUTES**

    *SymbolResults symbol* -- The symbol to be reformatted.
    It is assumed that this symbol CAN be reformatted
    (i.e. that we can reinterpret the utterances where these symbols 
    were spoken).

    *MediatorConsoleWX console* -- the MediatorConsole object which owns
    the correction box
    
    *wxDialog parent* -- the parent window.
    """
    def __init__(self, console, parent, **args):
            
        """
        **INPUTS**

        *MediatorConsoleWX console* -- the MediatorConsole object which owns
        the correction box

        *SymbolResults symbol* -- The symbol to be reformatted. It is assumed that 
        this symbol CAN be reformatted
        (i.e. that we can reinterpret the utterances where these symbols 
        were spoken).

        """
        possible_capture.__init__(self)
        self.deep_construct(ReformatFromRecentViewWX,
                            {
                             'console': console,
                             'symbol': None,
                            }, args, 
                            exclude_bases = {possible_capture:1, wxDialogWithHelpers: 1}
                           )
                           
        

        wxDialogWithHelpers.__init__(self, parent, wxNewId(), "Reformat a Symbol", wxDefaultPosition,
            (600, 400),
            style = wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER)
        
        self.name_parent('console')
        
        self.set_layout()
        return
        

    def set_layout(self):
        main_sizer = wxBoxSizer(wxVERTICAL)

        self.txt_intro = wxStaticText(self, wxNewId(), 
            "dummy\ndummy",
            wxDefaultPosition, wxDefaultSize)
        set_text_font(self.txt_intro)
        main_sizer.Add(self.txt_intro, 0, wxEXPAND | wxALL)

        self.txt_chosen_form = wxTextCtrl(self, wxNewId(), "", wxDefaultPosition,
            (550, 20), style = wxTE_NOHIDESEL)
        set_text_font(self.txt_chosen_form)
        main_sizer.Add(self.txt_chosen_form, 0, wxEXPAND | wxALL)
        
        formats_pick_list = wxListCtrlWithHelpers(self, wxNewId(), wxDefaultPosition,
            wxDefaultSize, 
            style = wxLC_REPORT | wxLC_HRULES | wxLC_SINGLE_SEL)
        set_text_font(formats_pick_list)
        
        formats_pick_list.InsertColumn(0, "")
        formats_pick_list.InsertColumn(1, "Written form")

        formats_pick_list.SetColumnWidth(0, wxLIST_AUTOSIZE_USEHEADER)
        formats_pick_list.SetColumnWidth(1, wxLIST_AUTOSIZE_USEHEADER)

        self.formats_pick_list = formats_pick_list
        main_sizer.Add(formats_pick_list, 1, wxEXPAND | wxALL)
        self.okb = wxButtonWithHelpers(self, wxID_OK, "OK", wxDefaultPosition, wxDefaultSize)
        self.cancelb = wxButtonWithHelpers(self, wxID_CANCEL, "Cancel", wxDefaultPosition, wxDefaultSize)
        b_sizer = wxBoxSizer(wxHORIZONTAL)
        b_sizer.Add(self.okb, 0, 0)
        b_sizer.Add(self.cancelb, 0, 0)
        main_sizer.Add(b_sizer, 0, wxEXPAND | wxALL)
        self.okb.SetDefault()

        self.SetAutoLayout(true)
        self.SetSizer(main_sizer)
        self.Layout()
        actual = main_sizer.GetSize()
        minimum = main_sizer.GetMinSize()
        list_size = self.formats_pick_list.GetSize()
        list_client_size = self.formats_pick_list.GetClientSize()
        h = list_client_size.GetHeight()
        w = list_client_size.GetWidth()
        main_sizer.SetItemMinSize(self.formats_pick_list, w, h)
        main_sizer.SetMinSize(wxSize(0, actual.GetHeight()))
        q = main_sizer.GetMinSize()
        main_sizer.Fit(self)
        main_sizer.SetMinSize(wxSize(q.GetWidth(), 0))
        resize_last_column(self.formats_pick_list)

        # AD: Keep those at the end, otherwise some of the code above
        #     could callback some Model actions which in turn could
        #     invoke the view before it has been defined.
        EVT_BUTTON(self, self.okb.GetId(), self.on_ok)
        EVT_BUTTON(self, self.cancelb.GetId(), self.on_cancel)
        EVT_LIST_ITEM_ACTIVATED(self.formats_pick_list, self.formats_pick_list.GetId(), 
                                self.on_choose_alternate_form)
        EVT_LIST_ITEM_SELECTED(self.formats_pick_list, self.formats_pick_list.GetId(), 
                               self.on_select_alternate_form)
        EVT_CHAR(self.formats_pick_list, self.on_format_pick_list_char)
        
# note: neither of these handlers gets called if a child control 
# has the focus.
# I thought they would be called if the focused control didn't have a
# handler
        EVT_ACTIVATE(self, self.on_activate)
        EVT_CHAR(self, self.on_dialog_char)


    def reset(self, symbol):
       """reinitialise the view layer
       
       **INPUTS**
       
       SymbolResult symbol -- The symbol to reinitialise with."""

       self.symbol = symbol
       spoken_form = string.join(self.symbol.spoken_phrase()),
       self.txt_intro.SetLabel("&Choose or type the correct format for symbol: \n     \"%s\"" % spoken_form)

       self.txt_chosen_form.SetValue(symbol.native_symbol())

       for ii in range(len(self.symbol.suggestions_list())):
           self.formats_pick_list.InsertStringItem(ii, "%d" % (ii+1))
           self.formats_pick_list.SetStringItem(ii, 1, self.symbol.suggestions_list()[ii])

       self.formats_pick_list.ScrollList(0, len(self.symbol.suggestions_list()))



       if self.formats_pick_list.NumberOfRows() > 0:
          self.formats_pick_list.Select(0)
          
       self.formats_pick_list.SetFocus()

       self.Raise()


    def intro(self):
       return self.txt_intro.GetLabel()

    def chosen_form(self):
       """returns the written form of the symbol being displayed by the 
       view layer"""
       return self.txt_chosen_form.GetValue()
       
    def on_select(self):
       debug.not_implemented('ReformatFromRecentViewWX.on_select')

    def on_double(self):
       debug.not_implemented('ReformatFromRecentViewWX.on_double')


    def on_ok(self, event=None):
       self.model().on_ok(event)
       self.EndModal(wxID_OK)

    def on_cancel(self, event=None):
       debug.trace('ReformatFromRecentViewWX.on_cancel', '** invoked, self=%s' % self)
       self.model().on_cancel(event)
       self.EndModal(wxID_CANCEL)

    def on_activate(self, event):
       pass

    def on_dialog_char(self, event):
       debug.not_implemented('ReformatFromRecentViewWX.on_dialog_char')

    def on_format_pick_list_char(self, event):
       self.model().on_format_pick_list_char(event)

    def on_choose_alternate_form(self, event):
       debug.trace('ReformatFromRecentViewWX.on_choose_alternate_form', 'invoked')
       self.model().on_choose_alternate_form(event.GetIndex())

    def displayed_list_of_alternate_forms(self):
       return self.formats_pick_list.GetColumnContents(1)
       
    def do_select_nth_form(self, nth):
       self.formats_pick_list.Select(nth)

    def do_choose_nth_form(self, nth):
# AD: Eventually, when we migrate to wxPython 2.5, we'll be able
# to invoke ActivateNth(). But for now it does not work in
# wxPython 2.4 and the upgrade to 2.5 is a real bitch.
# Delay it til after first release.
#       self.formats_pick_list.ActivateNth(nth)
       self.formats_pick_list.Select(nth)
       evt = MockListSelectionEvent(nth)
       self.on_choose_alternate_form(evt)


    def on_select_alternate_form(self, evt):
       debug.trace('ReformatFromRecentViewWX.on_select_alternate_form', 'invoked')
       self.model().on_select_alternate_form(evt.GetIndex())
       
    def set_alternate_form(self, written_form):
       self.txt_chosen_form.SetValue(written_form)
       
    def do_cancel(self):
       self.cancelb.Click()
       
    def do_ok(self):
       self.okb.Click()
       
    def do_type_form(self, form):
       self.set_alternate_form(form)

    def selected_alternate_form_index(self):
       debug.trace('ReformatFromRecentViewWX.selected_alternate_form_index', 'self.formats_pick_list.GetFirstSelected()=%s' % self.formats_pick_list.GetFirstSelected())
       return self.formats_pick_list.GetFirstSelected()
                     
    def cleanup(self):
       self.Destroy()

       
 
# defaults for vim - otherwise ignore
# vim:sw=4
