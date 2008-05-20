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
import Object, vc_globals
import debug
import WinSystem

# used to simulate hot key

import sr_interface


# these modules are from the win32all package


import win32api
import win32con
import win32gui
import pywintypes

"""MSWindows implementation of WinSystem interface for certain 
window-system specific tasks

**MODULE VARIABLES**


"""

class WasForegroundWindowMSW(WinSystem.WasForegroundWindow):
    """win32 implementation of WasForegroundWindow interface for 
    storing the current foreground window and restoring it to 
    the foreground later
    """
    def __init__(self, **args):
        """create an object which stores the current foreground
        window"""
        self.deep_construct(WasForegroundWindowMSW, {'handle': None}, args)
        self.handle = win32gui.GetForegroundWindow()

    def restore_to_foreground(self):
        """restores the window to the foreground"""
        for i in range(2):
            try:
                win32gui.SetForegroundWindow(self.handle)
            except pywintypes.error:
                debug.trace('WasForegroundWindowMSW.restore_to_foreground',
                    'unknown error from SetForegroundWindow')
            else:
                return


class WinSystemMSW(WinSystem.WinSystem):
    """abstract base class defining an interface for window-system
    specific tasks
    """
    def __init__(self, **args):
        self.deep_construct(WinSystemMSW, {}, args)
        if self.main_frame_handle:
            self.set_hot_key()

    def set_main_frame_handle(self, handle):
        """set the value of the main_frame_handle after construction

        *INT handle* -- the window-system specific ID for the
        main frame of the mediator application
        """
        WinSystem.WinSystem.set_main_frame_handle(self, handle)
        self.set_hot_key()

    def set_hot_key(self):
        handle = self.main_frame_handle
        HOTKEYF_SHIFT = 1
        HOTKEYF_CONTROL = 2
        HOTKEYF_ALT = 4
        key = win32con.VK_F8
        mods = HOTKEYF_SHIFT | HOTKEYF_CONTROL
        mods = mods * 256
        wParam = key | mods
        lParam = 0
        result = win32api.SendMessage(handle, win32con.WM_SETHOTKEY, wParam, lParam)
        if result == 2:
            debug.trace('WinSystemMSW.set_hot_key', 'hot key already in use')
        elif result != 1:
            debug.trace('WinSystemMSW.set_hot_key', 'failed to set hot key')

    def store_foreground_window(self):
        """detect the current foreground window, and store it in a
        WasForegroundWindow object, so that the window can later
        be restored to the foreground

        **INPUTS**

        *none*

        **OUTPUTS**

        *WasForegroundWindowMSW* -- the object which can be used to restore
        the window to the foreground
        """
        return WasForegroundWindowMSW()

    def raise_main_frame(self):
        """bring the main frame of the application (and any modal dialog
        boxes on top of it to the foreground

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        sr_interface.send_keys("{Ctrl+Shift+F8}", system = 1)

