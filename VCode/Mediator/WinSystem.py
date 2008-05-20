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


"""Defines abstract interface for certain window-system specific tasks

**MODULE VARIABLES**


"""

import sys
import Object, vc_globals
import debug

class WasForegroundWindow(Object.Object):
    """abstract base class defining an interface for storing the current
    foreground window and restoring it to the foreground later

    """
    def __init__(self, **args):
        """create an object which stores the current foreground
        window"""
        self.deep_construct(WasForegroundWindow, {}, args)

    def restore_to_foreground(self):
        """restores the window to the foreground"""
        debug.virtual('WasForegroundWindow.restore_to_foreground')


class WinSystem(Object.Object):
    """abstract base class defining an interface for window-system
    specific tasks

    **INSTANCE ATTRIBUTES**

    *INT main_frame_handle* -- the window-system specific ID for the
    main frame of the mediator application
    """
    def __init__(self, main_frame_handle = None, **args):
        """
        **INPUTS**

        *INT main_frame_handle* -- the window-system specific ID for the
        main frame of the mediator application
        """
        self.deep_construct(WinSystem, 
                            {
                             'main_frame_handle': main_frame_handle
                            }, args)

    def set_main_frame_handle(self, handle):
        """set the value of the main_frame_handle after construction

        *INT handle* -- the window-system specific ID for the
        main frame of the mediator application
        """
        self.main_frame_handle = handle

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
        debug.virtual('WinSystem.store_foreground_window')

    def raise_main_frame(self):
        """bring the main frame of the application (and any modal dialog
        boxes on top of it to the foreground

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        debug.virtual('WinSystem.raise_main_frame')

