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
# (C)2000, David C. Fox
#
##############################################################################

"""customized versions of wxSplitterWindow to provide common services"""


import debug
from Object import Object
from wxPython.wx import *

class wxAutoSplitterWindow(wxSplitterWindow, Object):
    """subclass of standard wxPython wxSplitterWindow, providing storage
    of the most recent split.

    **INSTANCE ATTRIBUTES**

    *INT* split_mode -- wxSPLIT_HORIZONTAL or wxSPLIT_VERTICAL

    *INT* previous_split -- location (in pixels) of previous sash split
  
    **CLASS ATTRIBUTES**

    *none*
    """
    def __init__(self, parent, ID, point = wxDefaultPosition, size =
        wxDefaultSize, style = wxSP_3D, name = "splitterWindow", **args):
        """initialize: same arguments as wxSplitterWindow

        **INPUTS**

        *wxWindow* parent -- parent window

        *wxWindowID* ID

        *wxPoint* point -- position of window

        *wxSize* size -- size of window

        *INT* style

        *STR* name
        """

        self.deep_construct(wxAutoSplitterWindow, {"previous_split":0,
            "split_mode": wxSPLIT_HORIZONTAL}, args, exclude_bases =
            {wxSplitterWindow: 1})
# initialize base class manually
        wxSplitterWindow.__init__(self, parent, ID, point, size, style, name)

        if self.IsSplit():
            self.split_mode = self.GetSplitMode()
            self.previous_split = self.GetSashPosition()
        EVT_SPLITTER_UNSPLIT(self, ID, self.unsplit)
        EVT_SPLITTER_SASH_POS_CHANGED(self, ID, self.save_previous_split)
        EVT_SIZE(self, self.on_size)
    
    def on_size(self, event):
        size = event.GetSize()
#        print 'sizing pane'
#        print size.GetWidth(), size.GetHeight()
#        print self.GetClientSizeTuple()
        event.Skip()
    def restore_split(self):
        """restores splitter sash to previous location, re-splitting the
        window if one half has been hidden.

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
    
        if not self.IsSplit():
            first = self.GetWindow1()
            second = self.GetWindow2()
# can't restore split unless both windows are present
            if ( first and second):
                if ( self.split_mode == wxSPLIT_HORIZONTAL):
                    self.SplitHorizontally(first, second, self.previous_split)
                else:
                    self.SplitVertically(first, second, self.previous_split)
        else:
            self.SetSplitPosition(self.previous_split)

    def unsplit(self, event):
        """event handler for UNSPLIT event
        
        **INPUTS**
        
        *wxSplitterEvent* event
        
        """
        self.previous_split = self.GetSashPosition()
        self.split_mode = self.GetSplitMode()
        event.Skip()

    def save_previous_split(self, event):
        """event handler for EVT_SPLITTER_SASH_POS_CHANGED event

        **INPUTS**
    
        *wxSplitterEvent* event
        """
        self.previous_split = event.GetSashPosition()
        self.split_mode = self.GetSplitMode()
        event.Skip()

class wxFixedFocusSplitter(wxAutoSplitterWindow):
    """subclass of wxAutoSplitterWindow, which adds memory of which
    child should accept the focus

    **INSTANCE ATTRIBUTES**

    *INT* fixed_focus -- which window (1 or 2) should accept the focus
    on an EVT_SET_FOCUS event
  
    **CLASS ATTRIBUTES**

    *none*
    """
    def __init__(self, parent, ID, fixed_focus = 1, **args):
        """initialize: same arguments as wxAutoSplitterWindow

        **INPUTS**

        *wxWindow* parent -- parent window

        *wxWindowID* ID

        *INT* fixed_focus -- which window (1 or 2) should accept the focus
        on an EVT_SET_FOCUS event
        """

# initialize base class
        args["parent"] = parent
        args["ID"] = ID
        self.deep_construct( wxFixedFocusSplitter, 
            {"fixed_focus": fixed_focus}, args)
        EVT_SET_FOCUS(self, self.on_focus)

    def on_focus(self, event):
        """event handler for SET_FOCUS event
        
        **INPUTS**
        
        *wxFocusEvent* event
        
        """
        if ( self.fixed_focus == 2):
            second = self.GetWindow2()
            if second and second.IsShown():
                second.SetFocus()
                return
        first = self.GetWindow1()
        if first and first.IsShown():
            first.SetFocus()

    def set_default_focus(self, fixed_focus):
        """changes the window which will receive focus on SET_FOCUS.
        Note: set_default_focus only changes the default to be used on
        the next SET_FOCUS event.  It does not update the current focus.

        **INPUTS**

        *INT* fixed_focus -- which window (1 or 2) should accept the focus
        on an EVT_SET_FOCUS event

        **OUTPUTS**

        *none*
        """
        self.fixed_focus = fixed_focus
