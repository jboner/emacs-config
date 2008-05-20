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

"""interface for windows which are known to be targets of VoiceCode-managed
dictation
"""

import debug
import string
import re
from Object import Object
import messaging

class TargetWindow(Object):
    """abstract class defining interface for finding the active editor
    application corresponding to a known window

    **INSTANCE ATTRIBUTES**

    *INT* window -- unique integer identifying this window to 
    the operating system

    *STR* module -- name of the executable module associated with this
    window

    *[STR]* window_instances -- list of associated instance names, sorted in 
    the order of instances most recently known to be active
    
    *BOOL* single_window_display -- indicates whether the window is
    associated with a module which displays multiple remote windows in a
    single local window

    **CLASS ATTRIBUTES**
    
    *none*
    """
    def __init__(self, window, module, instance = None, single_display = 0, 
        **args):
        """abstract class, so this is only called by subclass
        constructor.

        **INPUTS**

        *INT* window -- unique integer identifying this window to 
        the operating system

        *STR* module -- name of the executable module associated with this
        window

        *STR* instance -- name of initial instance associated with this window
        (or None if no initial instance)

        **OUTPUTS**

        *none*
        """
        self.deep_construct(TargetWindow,
                            { 'window': window,
                              'module': module,
                              'window_instances': [],
                              'single_window_display': single_display
                            },
                            args)
        if instance != None:
            self.window_instances = [instance]

    def add_instance(self, instance):
        """add a new instance to the window

        **INPUTS**

        *STR* instance -- name of the editor instance

        **OUTPUTS**

        *BOOL* -- true if the instance was added successfully
        """
        debug.virtual('TargetWindow.add_instance')

    def delete_instance(self, instance):
        """notify TargetWindow that an editor instance has been deleted
    
        **INPUTS**

        *STR* instance -- name of the editor instance

        **OUTPUTS**

        *BOOL* -- true if instance was known to be associated with this
        window
        """
        try:
            self.window_instances.remove(instance)
        except ValueError:
            return 0
        return 1

    def known_instance(self, instance):
        """is instance known?

        **INPUTS**

        *STR* instance -- name of the application instance 

        **OUTPUTS**

        *BOOL* -- true if instance is known 
        """
        if instance in self.instance_names():
            return 1
        return 0

    def module_name(self):
        """returns the module name corresponding to this window

        **INPUTS**

        *none*

        **OUTPUTS**

        *STR* -- the module name
        """
        return self.module

    def instance_names(self):
        """returns the list of known instances associated with the
        window, in order of the most recently active instance

        **INPUTS**

        *none*

        **OUTPUTS**

        *[STR]* -- the instance names
        """
        return self.window_instances

    def instances(self):
        """returns the number of known instances associated with the
        window

        **INPUTS**

        *none*

        **OUTPUTS**

        *INT* -- how many instances are associated with this window
        """
        return len(self.window_instances)

    def shared(self):
        """can this window be shared by multiple instances, or is it a
        dedicated editor window?  **Note:** shared should return true if
        the window is a shareable shell window, even if there is only
        one instance currently associated with the window.

        **INPUTS**

        *none*

        **OUTPUTS**

        *BOOL* -- is window shareable?
        """
        debug.virtual('TargetWindow.shared')

    def single_display(self):
        """does this module display multiple remote windows in this single 
        local window?  **Note:** single_display is not consistent with a
        dedicated module.

        **INPUTS**

        *INT* window -- handle of the local window

        *STR* title -- title of the local window

        **OUTPUTS**

        *BOOL* -- is this a single-window display?
        """
        return self.single_window_display

    def active_instance(self, title, editors, default_to_recent = 1):
        """attempts to determine the name of the active instance
        currently associated with this window

        **INPUTS**

        *STR* title -- the current title of the window

        *AppMgr* editors -- application manager for associating instance
        names with applications

        *BOOL* default_to_recent -- controls behavior of 
        active_instance when it is unable to determine which instance is
        active.  If true, active_instance will
        default to the most recently known active instance.  Otherwise,
        it will return none.

        **OUTPUTS**

        *STR* -- the active instance name, or None if no known
        instance appears to be active in this window
        """
        debug.virtual('TargetWindow.active_instance')

    def activate_instance(self, instance):
        """raise instance to front of list of most recently active instances 
        for this window

        **INPUTS**

        *STR* instance -- name of the application instance 

        **OUTPUTS**

        *BOOL* -- true if instance is known (otherwise, does
        nothing)
        """
        if not self.known_instance(instance):
            return 0
        try:
            self.window_instances.remove(instance)
            self.window_instances.insert(0, instance)
        except ValueError:
            return 0
        return 1

    def verify_new_instance(self, title, instance, editors):
        """attempt to verify whether this window belongs to
        the named instance.  

        **INPUTS**

        *STR* title -- title of the window 

        *STR* instance -- name of the editor instance

        *AppMgr* editors -- the AppMgr object

        **OUTPUTS**

        *BOOL* -- true if verify_new_instance was able to determine that the
        window was associated with the named instance, false if it
        was able to determine that the window was not associated
        with the instance, and None if it was unable to determine.
        """
        debug.virtual('TargetWindow.verify_new_instance')
        

class DedicatedWindow(TargetWindow):
    """window dedicated to a particular editor instance

    **INSTANCE ATTRIBUTES**

    *none*

    **CLASS ATTRIBUTES**
    
    *none*
    """
    def __init__(self, **args):
        self.deep_construct(DedicatedWindow,
                            {},
                            args)

    def shared(self):
        """can this window be shared by multiple instances, or is it a
        dedicated editor window?  **Note:** shared should return true if
        the window is a shareable shell window, even if there is only
        one instance currently associated with the window.

        **INPUTS**

        *none*

        **OUTPUTS**

        *BOOL* -- is window shareable?
        """
        return 0

    def add_instance(self, instance):
        """add a new instance to the window

        **INPUTS**

        *STR* instance -- name of the editor instance

        **OUTPUTS**

        *BOOL* -- true if the instance was added successfully
        """
# only one instance, can't add another
        return 0 

    def active_instance(self, title, editors, default_to_recent = 1):
        """attempts to determine the name of the active instance
        currently associated with this window

        **INPUTS**

        *STR* title -- the current title of the window

        *AppMgr* editors -- application manager for associating instance
        names with applications

        *BOOL* default_to_recent -- controls behavior of 
        active_instance when it is unable to determine which instance is
        active.  If true, active_instance will
        default to the most recently known active instance.  Otherwise,
        it will return none.

        **OUTPUTS**

        *STR* -- the active instance name, or None if no known
        instance appears to be active in this window
        """
        if self.instances() == 0:
# should never be true  (for a dedicated window)
            return None
# only one instance, can't be suspended 
        return self.window_instances[0]

    def verify_new_instance(self, title, instance, editors):
        """attempt to verify whether this window belongs to
        the named instance.  

        **INPUTS**

        *STR* title -- title of the window 

        *STR* instance -- name of the editor instance

        *AppMgr* editors -- the AppMgr object

        **OUTPUTS**

        *BOOL* -- true if verify_new_instance was able to determine that the
        window was associated with the named instance, false if it
        was able to determine that the window was not associated
        with the instance, and None if it was unable to determine.
        """
# dedicated windows can't have new instances
        return 0
        

class SharedWindow(TargetWindow):
    """shared (shell) window which may have more than one application
    instance (though only one may be active at a time)

    **INSTANCE ATTRIBUTES**

    *BOOL* variable_title -- can an editor change the title of this
    window?

    *none*

    **CLASS ATTRIBUTES**
    
    *none*
    """
    def __init__(self, variable_title = 1, **args):
        self.deep_construct(SharedWindow,
                            {'variable_title': variable_title},
                            args)

    def shared(self):
        """can this window be shared by multiple instances, or is it a
        dedicated editor window?  **Note:** shared should return true if
        the window is a shareable shell window, even if there is only
        one instance currently associated with the window.

        **INPUTS**

        *none*

        **OUTPUTS**

        *BOOL* -- is window shareable?
        """
        return 1

    def add_instance(self, instance):
        """add a new instance to the window

        **INPUTS**

        *STR* instance -- name of the editor instance

        **OUTPUTS**

        *BOOL* -- true if the instance was added successfully
        """
        if self.known_instance(instance):
            return 0
        self.window_instances.insert(0, instance)
        return 1

    def active_instance(self, title, editors, default_to_recent = 1):
        """attempts to determine the name of the active instance
        currently associated with this window

        **INPUTS**

        *STR* title -- the current title of the window

        *AppMgr* editors -- application manager for associating instance
        names with applications

        *BOOL* default_to_recent -- controls behavior of 
        active_instance when it is unable to determine which instance is
        active.  If true, active_instance will
        default to the most recently known active instance.  Otherwise,
        it will return none.

        **OUTPUTS**

        *STR* -- the active instance name, or None if no known
        instance appears to be active in this window
        """
        if self.instances() == 0:
            return None
        unknown_instance = None
        for instance in self.window_instances:
            app = editors.app_instance(instance)
            if app.is_active_is_safe():
                try:
                    active = app.is_active()
                except messaging.SocketError:
                    editors.close_app_cbk(instance, unexpected = 1)
                    continue
                if active:
                    return instance
                else:
# if not active, skip to the next app, but if we can't tell, try an
# alternative test
                    continue
            elif self.variable_title:
# alternate means of checking whether the application is active
                s = app.instance_string()
                if s != None:
                    if string.find(title, s) != -1:
                        return instance
                    else:
                        continue
            if unknown_instance == None:
                unknown_instance = instance
# if no instance is known to be active, we return either None
# or the most recently active instance, depending on the value 
# of default_to_recent.
        if not default_to_recent:
            return None
        return unknown_instance

    def verify_new_instance(self, title, instance, editors):
        """attempt to verify whether this window belongs to
        the named instance.  

        **INPUTS**

        *STR* title -- title of the window 

        *STR* instance -- name of the editor instance

        *AppMgr* editors -- the AppMgr object

        **OUTPUTS**

        *BOOL* -- true if verify_new_instance was able to determine that the
        window was associated with the named instance, false if it
        was able to determine that the window was not associated
        with the instance, and None if it was unable to determine.
        """
        if self.variable_title:
            app = editors.app_instance(instance)
            s = app.instance_string()
            if s != None:
                if string.find(title, s) != -1:
                    return 1
                else:
                    return 0
# otherwise, just try to check if another instance is known to be active
        if self.active_instance(title, editors, default_to_recent = 0) != None:
# another instance is active, so this can't be the right window
            return 0
# otherwise, unknown
        return None
                
# defaults for vim - otherwise ignore
# vim:sw=4

