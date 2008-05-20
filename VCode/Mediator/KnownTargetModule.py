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

"""abstract class defining interface for an object which receives 
recognition-starting (or onBegin/gotBegin) callbacks, figures out which
application and buffer are active, and tells the GramMgr to activate the
appropriate grammars.
"""

import debug
import string
import re
from Object import Object

import TargetWindow

class KnownTargetModule(Object):
    """abstract class defining interface to provide information about
    modules (local executables) which are or contain
    VoiceCode-manageable editors

    **INSTANCE ATTRIBUTES**

    *STR* module_name -- name of the module, as detected by
    RecogStartMgr

    *(STR, STR)* title_escape -- the escape sequences to be sent before 
    and after the the string to place in the window title, or None
    if there is no known escape sequence

    **CLASS ATTRIBUTES**
    
    *none*
    """
    def __init__(self, module_name, title_escape = None, **args):
        """

        **INPUTS**
    
        *STR* module_name -- name of the module, as detected by
        RecogStartMgr

        *(STR, STR)* title_escape -- the escape sequences to be sent before 
        and after the the string to place in the window title, or None
        if there is no known escape sequence

        **OUTPUTS**

        *none*
        """
        self.deep_construct(KnownTargetModule, 
                            {'module_name': string.lower(module_name),
                             'title_escape': title_escape
                            }, 
                            args)

    def name(self):
        """return the name of the module

        **INPUTS**

        *none*

        **OUTPUTS**

        *STR* -- name of the module
        """
        return self.module_name
    
    def dedicated(self):
        """is this module dedicated to a specific (local) editor, or is it a
        (local or remote) shell in which many programs can be run?

        **INPUTS**

        *none*

        **OUTPUTS**

        *BOOL* -- is window dedicated to a specific editor
        """
        debug.virtual('KnownTargetModule.dedicated')

    def editor(self):
        """if dedicated, returns VoiceCode's name for the corresponding
        editor

        **INPUTS**

        *none*

        **OUTPUTS**

        *STR* -- VoiceCode's internal name for the editor corresponding
        to this module (or None if not dedicated)
        """
        debug.virtual('KnownTargetModule.editor')

    def single_display(self, window, title):
        """does this module display multiple remote windows in this single 
        local window?  **Note:** single_display is not consistent with a
        dedicated module.

        **INPUTS**

        *INT* window -- handle of the local window

        *STR* title -- title of the local window

        **OUTPUTS**

        *BOOL* -- is this a single-window display?
        """
        debug.virtual('KnownTargetModule.single_window')

    def variable_title(self):
        """can an editor running in this module change the window title?

        **INPUTS**

        *none*

        **OUTPUTS**

        *BOOL* -- true if the module allows the editor to change the
        window title
        """
        debug.virtual('KnownTargetModule.variable_title')

    def title_escape_sequence(self):
        """returns the escape sequence which can be used to set the 
        module's window title, if any.  

        **INPUTS**

        *none*

        **OUTPUTS**

        *(STR, STR)* title_escape -- the escape sequences to be sent before 
        and after the the string to place in the window title.  If
        there is no known escape sequence, both strings will be set to
        the empty string.
        """
        if self.title_escape == None:
            return ("", "")
        return self.title_escape

    def new_window(self, window, title, editors, instance_name = None):
        """factory which creates a new TargetWindow object.
        **Note:** the caller should first check single_display.  If it
        returns true, then the module cannot act as a window factory.
        Instead, the subwindow identification client will play that role.
        **Also note:** some subclasses will be unable to create a new
        window without an initial instance.

        **INPUTS**

        *INT* window -- the window handle (unique identifier) of the
        window

        *STR* title -- the current title of the window

        *AppMgr* editors -- the application manager
         
        *STR* instance -- the name of the initial instance belonging to
        the window, or None if there is none initially

        **OUTPUTS**

        *TargetWindow* -- a new TargetWindow object, or None
        if the module is unable to create one
        """
        debug.virtual('KnownTargetModule.new_window')
    
    def verify_new_instance(self, window, title, instance, editors):
        """attempt to verify whether the window belongs to
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
        debug.virtual('KnownTargetModule.verify_new_instance')
        
class LocalInterpreter(KnownTargetModule):
    """implements KnownTargetModule for a local interpreter which can
    run different applications (including internal editors)
    but only one active application per window.  At the moment, this
    class is functionally identical to RemoteShell, but at some point
    there might be other methods which distinguish the two.

    **INSTANCE ATTRIBUTES**

    *BOOL* title_varies -- can the title of the window be changed?

    **CLASS ATTRIBUTES**
    
    *none*
    """
    def __init__(self, title_varies = 0, **args):
        """ 
        **INPUTS**

        *BOOL* title_varies -- can the title of the window be changed?

        **OUTPUTS**

        *none*
        """
        self.deep_construct(LocalInterpreter, 
                            {'title_varies': title_varies}, args)

    def dedicated(self):
        """is this module dedicated to a specific (local) editor, or is it a
        (local or remote) shell in which many programs can be run?

        **INPUTS**

        *none*

        **OUTPUTS**

        *BOOL* -- is window dedicated to a specific editor
        """
        return 0

    def editor(self):
        """if dedicated, returns VoiceCode's name for the corresponding
        editor

        **INPUTS**

        *none*

        **OUTPUTS**

        *STR* -- VoiceCode's internal name for the editor corresponding
        to this module (or None if not dedicated)
        """
        return None

    def single_display(self, window, title):
        """does this module display multiple remote windows in this single 
        local window?  **Note:** single_display is not consistent with a
        dedicated module.

        **INPUTS**

        *INT* window -- handle of the local window

        *STR* title -- title of the local window

        **OUTPUTS**

        *BOOL* -- is this a single-window display?
        """
        return 0

    def variable_title(self):
        """can an editor running in this module change the window title?

        **INPUTS**

        *none*

        **OUTPUTS**

        *BOOL* -- true if the module allows the editor to change the
        window title
        """
        return self.title_varies

    def new_window(self, window, title, editors, instance_name = None):
        """factory which creates a new TargetWindow object.
        **Note:** the caller should first check single_display.  If it
        returns true, then the module cannot act as a window factory.
        Instead, the subwindow identification client will play that role.
        **Also note:** some subclasses will be unable to create a new
        window without an initial instance.

        **INPUTS**

        *INT* window -- the window handle (unique identifier) of the
        window

        *STR* title -- the current title of the window

        *AppMgr* editors -- the application manager
         
        *STR* instance_name -- the name of the initial instance belonging to
        the window, or None if there is none initially

        **OUTPUTS**

        *TargetWindow* -- a new TargetWindow object, or None
        if the module is unable to create one
        """
        return TargetWindow.SharedWindow(window = window, 
            module = self.name(), 
            instance = instance_name, 
            single_display = self.single_display(window, title), 
            variable_title = self.variable_title())
    
    def verify_new_instance(self, window, title, instance, editors):
        """attempt to verify whether the window belongs to
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
        if self.variable_title():
            app = editors.app_instance(instance)
            s = app.instance_string()
            if s != None:
                if string.find(title, s) != -1:
                    return 1
                else:
                    return 0
# otherwise, unknown
        return None
                

class RemoteShell(KnownTargetModule):
    """implements KnownTargetModule for a remote shell which can run
    different applications, but only one active application per window
    (cf. [RemoteDisplay])

    **INSTANCE ATTRIBUTES**

    *BOOL* title_varies -- can the title of the window be changed?

    **CLASS ATTRIBUTES**
    
    *none*

    .. [RemoteDisplay] file:///./RecogStartMgr.RecogStartMgr.html#RemoteDisplay
    """
    def __init__(self, title_varies = 0, **args):
        """ 
        **INPUTS**

        *BOOL* title_varies -- can the title of the window be changed?

        **OUTPUTS**

        *none*
        """
        self.deep_construct(RemoteShell, {'title_varies': title_varies}, args)

    def dedicated(self):
        """is this module dedicated to a specific (local) editor, or is it a
        (local or remote) shell in which many programs can be run?

        **INPUTS**

        *none*

        **OUTPUTS**

        *BOOL* -- is window dedicated to a specific editor
        """
        return 0

    def editor(self):
        """if dedicated, returns VoiceCode's name for the corresponding
        editor

        **INPUTS**

        *none*

        **OUTPUTS**

        *STR* -- VoiceCode's internal name for the editor corresponding
        to this module (or None if not dedicated)
        """
        return None

    def single_display(self, window, title):
        """does this module display multiple remote windows in this single 
        local window?  **Note:** single_display is not consistent with a
        dedicated module.

        **INPUTS**

        *INT* window -- handle of the local window

        *STR* title -- title of the local window

        **OUTPUTS**

        *BOOL* -- is this a single-window display?
        """
        return 0

    def variable_title(self):
        """can an editor running in this module change the window title?

        **INPUTS**

        *none*

        **OUTPUTS**

        *BOOL* -- true if the module allows the editor to change the
        window title
        """
        return self.title_varies

    def new_window(self, window, title, editors, instance_name = None):
        """factory which creates a new TargetWindow object.
        **Note:** the caller should first check single_display.  If it
        returns true, then the module cannot act as a window factory.
        Instead, the subwindow identification client will play that role.
        **Also note:** some subclasses will be unable to create a new
        window without an initial instance.

        **INPUTS**

        *INT* window -- the window handle (unique identifier) of the
        window

        *STR* title -- the current title of the window

        *AppMgr* editors -- the application manager
         
        *STR* instance_name -- the name of the initial instance belonging to
        the window, or None if there is none initially

        **OUTPUTS**

        *TargetWindow* -- a new TargetWindow object, or None
        if the module is unable to create one
        """
        return TargetWindow.SharedWindow(window = window, 
            module = self.name(), 
            instance = instance_name, 
            single_display = self.single_display(window, title), 
            variable_title = self.variable_title())
    
    def verify_new_instance(self, window, title, instance, editors):
        """attempt to verify whether the window belongs to
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
        if self.variable_title():
            app = editors.app_instance(instance)
            s = app.instance_string()
            if s != None:
                if string.find(title, s) != -1:
                    return 1
                else:
                    return 0
# otherwise, unknown
        return None
                
        


class DedicatedModule(KnownTargetModule):
    """implements KnownTargetModule for a module dedicated to a
    particular editor

    **INSTANCE ATTRIBUTES**

    *none*

    **CLASS ATTRIBUTES**
    
    *none*

    """
    def __init__(self, editor, **args):
        """ 
        **INPUTS**

        *STR* editor -- VoiceCode's internal name for the editor to
        which this module is dedicated

        **OUTPUTS**

        *none*
        """
        self.deep_construct(DedicatedModule, {'editor_name': editor}, args)

    def dedicated(self):
        """is this module dedicated to a specific (local) editor, or is it a
        (local or remote) shell in which many programs can be run?

        **INPUTS**

        *none*

        **OUTPUTS**

        *BOOL* -- is window dedicated to a specific editor
        """
        return 1

    def editor(self):
        """if dedicated, returns VoiceCode's name for the corresponding
        editor

        **INPUTS**

        *none*

        **OUTPUTS**

        *STR* -- VoiceCode's internal name for the editor corresponding
        to this module (or None if not dedicated)
        """
        return self.editor_name

    def single_display(self, window, title):
        """does this module display multiple remote windows in this single 
        local window?  **Note:** single_display is not consistent with a
        dedicated module.

        **INPUTS**

        *INT* window -- handle of the local window

        *STR* title -- title of the local window

        **OUTPUTS**

        *BOOL* -- is this a single-window display?
        """
        return 0

    def variable_title(self):
        """can an editor running in this module change the window title?

        **INPUTS**

        *none*

        **OUTPUTS**

        *BOOL* -- true if the module allows the editor to change the
        window title
        """
# for dedicated windows, we will query the editor to see if the title
# can vary
        return 1


    def new_window(self, window, title, editors, instance_name = None):
        """factory which creates a new TargetWindow object.
        **Note:** the caller should first check single_display.  If it
        returns true, then the module cannot act as a window factory.
        Instead, the subwindow identification client will play that role.
        **Also note:** some subclasses will be unable to create a new
        window without an initial instance.

        **INPUTS**

        *INT* window -- the window handle (unique identifier) of the
        window

        *STR* title -- the current title of the window

        *AppMgr* editors -- the application manager
         
        *STR* instance_name -- the name of the initial instance belonging to
        the window, or None if there is none initially

        **OUTPUTS**

        *TargetWindow* -- a new TargetWindow object, or None
        if the module is unable to create one
        """
        single = self.single_display(window, title)
        return TargetWindow.DedicatedWindow(window = window, 
            module = self.name(), instance = instance_name, 
            single_display = single)
    
    def verify_new_instance(self, window, title, instance, editors):
        """attempt to verify whether the window belongs to
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
        if self.variable_title():
            app = editors.app_instance(instance)
#            print 'verify new instance'
#            print instance, app
            s = app.instance_string()
            if s != None:
                if string.find(title, s) != -1:
                    return 1
                else:
                    return 0
# otherwise, unknown
        return None
                
        


class RemoteDisplay(KnownTargetModule):
    """abstract class for remote GUI displays which display one or more
    remote windows, either as separate local windows or within a single
    local window, with the possibility that multiple applications will
    be active in separate remote windows

    **INSTANCE ATTRIBUTES**

    *none*

    **CLASS ATTRIBUTES**
    
    *none*

    """
    def __init__(self, **args):
        """ 
        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        self.deep_construct(RemoteDisplay, {}, args)

    def dedicated(self):
        """is this module dedicated to a specific (local) editor, or is it a
        (local or remote) shell in which many programs can be run?

        **INPUTS**

        *none*

        **OUTPUTS**

        *BOOL* -- is window dedicated to a specific editor
        """
        return 0

    def editor(self):
        """if dedicated, returns VoiceCode's name for the corresponding
        editor

        **INPUTS**

        *none*

        **OUTPUTS**

        *STR* -- VoiceCode's internal name for the editor corresponding
        to this module (or None if not dedicated)
        """
        return None


class SingleWindowDisplay(RemoteDisplay):
    """implements RemoteDisplay(KnownTargetModule) for a remote display with
    all remote windows sharing a single local window

    **INSTANCE ATTRIBUTES**

    *none*

    **CLASS ATTRIBUTES**
    
    *none*

    """
    def __init__(self, **args):
        """ 
        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        self.deep_construct(SingleWindowDisplay, {}, args)

    def single_display(self, window, title):
        """does this module display multiple remote windows in this single 
        local window?  **Note:** single_display is not consistent with a
        dedicated module.

        **INPUTS**

        *INT* window -- handle of the local window

        *STR* title -- title of the local window

        **OUTPUTS**

        *BOOL* -- is this a single-window display?
        """
        return 1

    def variable_title(self):
        """can an editor running in this module change the window title?

        **INPUTS**

        *none*

        **OUTPUTS**

        *BOOL* -- true if the module allows the editor to change the
        window title
        """
        return 0
    
    def new_window(self, window, title, editors, instance_name = None):
        """factory which creates a new TargetWindow object.
        **Note:** the caller should first check single_display.  If it
        returns true, then the module cannot act as a window factory.
        Instead, the subwindow identification client will play that role.
        **Also note:** some subclasses will be unable to create a new
        window without an initial instance.

        **INPUTS**

        *INT* window -- the window handle (unique identifier) of the
        window

        *STR* title -- the current title of the window

        *AppMgr* editors -- the application manager
         
        *STR* instance -- the name of the initial instance belonging to
        the window, or None if there is none initially

        **OUTPUTS**

        *TargetWindow* -- a new TargetWindow object, or None
        if the module is unable to create one
        """
# SingleWindowDisplay needs a  subwindow identification client, which
# will act as the window factory
        return None
    
    def verify_new_instance(self, window, title, instance, editors):
        """attempt to verify whether the window belongs to
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
# should never be called when single_display is true, but just in case
        return None
                
        


class MultiWindowDisplay(RemoteDisplay):
    """implements RemoteDisplay(KnownTargetModule) for a remote display with
    each remote windows having its own local window

    **INSTANCE ATTRIBUTES**

    *BOOL* title_varies -- can the title of the window be changed?

    **CLASS ATTRIBUTES**
    
    *none*

    """
    def __init__(self, title_varies = 1, **args):
        """ 
        **INPUTS**

        *BOOL* title_varies -- can the title of the window be changed?

        **OUTPUTS**

        *none*
        """
        self.deep_construct(MultiWindowDisplay, 
                            {'title_varies': title_varies}, args)

    def single_display(self, window, title):
        """does this module display multiple remote windows in this single 
        local window?  **Note:** single_display is not consistent with a
        dedicated module.

        **INPUTS**

        *INT* window -- handle of the local window

        *STR* title -- title of the local window

        **OUTPUTS**

        *BOOL* -- is this a single-window display?
        """
        return 0

    def variable_title(self):
        """can an editor running in this module change the window title?

        **INPUTS**

        *none*

        **OUTPUTS**

        *BOOL* -- true if the module allows the editor to change the
        window title
        """
        return self.title_varies

    
    def new_window(self, window, title, editors, instance_name = None):
        """factory which creates a new TargetWindow object.
        **Note:** the caller should first check single_display.  If it
        returns true, then the module cannot act as a window factory.
        Instead, the subwindow identification client will play that role.
        **Also note:** some subclasses will be unable to create a new
        window without an initial instance.

        **INPUTS**

        *INT* window -- the window handle (unique identifier) of the
        window

        *STR* title -- the current title of the window

        *AppMgr* editors -- the application manager
         
        *STR* instance -- the name of the initial instance belonging to
        the window, or None if there is none initially

        **OUTPUTS**

        *TargetWindow* -- a new TargetWindow object, or None
        if the module is unable to create one
        """
# without an instance, we can't distinguish between dedicated and shared
# windows, so we can't create the appropriate TargetWindow
        if instance_name == None or not editors.known_instance(instance_name):
            return None
        instance = editors.app_instance(instance_name)
        if instance.shared_window():
            return TargetWindow.SharedWindow(window = window, 
                module = self.name(), 
                instance = instance_name,
                single_display = self.single_display(window, title), 
                variable_title = self.variable_title)
        return TargetWindow.DedicatedWindow(window = window, 
            module = self.name(), 
            instance = instance_name,
            single_display = self.single_display(window, title))
    
    def verify_new_instance(self, window, title, instance, editors):
        """attempt to verify whether the window belongs to
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
        if self.variable_title():
            app = editors.app_instance(instance)
            s = app.instance_string()
            if s != None:
                if string.find(title, s) != -1:
                    return 1
                else:
                    return 0
# otherwise, unknown
        return None
                
        

class DualModeDisplay(RemoteDisplay):
    """abstract subclass of RemoteDisplay(KnownTargetModule) for 
    a remote display which can operate in either single-window or
    multi-window modes, with the mode detectable at run-time

    **INSTANCE ATTRIBUTES**

    *none*

    **CLASS ATTRIBUTES**
    
    *none*

    """
    def __init__(self, **args):
        """ 
        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        self.deep_construct(DualModeDisplay, {}, args)
    
    def new_window(self, window, title, editors, instance_name = None):
        """factory which creates a new TargetWindow object.
        **Note:** the caller should first check single_display.  If it
        returns true, then the module cannot act as a window factory.
        Instead, the subwindow identification client will play that role.
        **Also note:** some subclasses will be unable to create a new
        window without an initial instance.

        **INPUTS**

        *INT* window -- the window handle (unique identifier) of the
        window

        *STR* title -- the current title of the window

        *AppMgr* editors -- the application manager
         
        *STR* instance -- the name of the initial instance belonging to
        the window, or None if there is none initially

        **OUTPUTS**

        *TargetWindow* -- a new TargetWindow object, or None
        if the module is unable to create one
        """
# if this is a single-window display, we need a subwindow identification
# client to create a TargetWindow object
        if self.single_display(window, title):
            return None
# without an instance, we can't distinguish between dedicated and shared
# windows, so we can't create the appropriate TargetWindow
        if instance_name == None or not editors.known_instance(instance_name):
            return None
        instance = editors.app_instance(instance_name)
        if instance.shared_window():
            return TargetWindow.SharedWindow(window = window, 
                module = self.name(), 
                instance = instance_name,
                single_display = self.single_display(window, title), 
                variable_title = self.variable_title)
        return TargetWindow.DedicatedWindow(window = window, 
            module = self.name(), 
            instance = instance_name,
            single_display = self.single_display(window, title))

    def verify_new_instance(self, window, title, instance, editors):
        """attempt to verify whether the window belongs to
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
        if self.single_display(window, title):
            return None
        if self.variable_title():
            app = editors.app_instance(instance)
            s = app.instance_string()
            if s != None:
                if string.find(title, s) != -1:
                    return 1
                else:
                    return 0
# otherwise, unknown
        return None
                
        

class DualModeDisplayByTitle(DualModeDisplay):
    """implements DualModeDisplay for a module where the mode can be
    detected by checking the window title

    **INSTANCE ATTRIBUTES**

    *STR* title_regex -- regular expression compared against the window
    title to see if this remote display is running in single-window mode

    *BOOL* title_varies -- can the title of the window be changed (when
    in multi-window mode?

    **CLASS ATTRIBUTES**
    
    *none*

    """
    def __init__(self, title_regex, title_varies = 1, **args):
        """ 
        **INPUTS**

        *STR* title_regex -- regular expression compared against the window
        title to see if this remote display is running in single-window mode
        **Note:** If the title must be an exact match, title_regex
        should include ^ and $ explicitly to require matches only at the
        beginning and end of the title string.  (?i) can be used to
        specify a case-insensitive search.

        *BOOL* title_varies -- can the title of the window be changed (when
        in multi-window mode?

        **OUTPUTS**

        *none*
        """
        self.deep_construct(DualModeDisplayByTitle, 
                            {'title_regex': title_regex,
                             'title_varies': title_varies},
                            args)

    def single_display(self, window, title):
        """does this module display multiple remote windows in this single 
        local window?  **Note:** single_display is not consistent with a
        dedicated module.

        **INPUTS**

        *INT* window -- handle of the local window

        *STR* title -- title of the local window

        **OUTPUTS**

        *BOOL* -- is this a single-window display?
        """
        if re.search(self.title_regex, title) == None:
            return 0
        return 1

    def variable_title(self):
        """can an editor running in this module change the window title?

        **INPUTS**

        *none*

        **OUTPUTS**

        *BOOL* -- true if the module allows the editor to change the
        window title
        """
        return self.title_varies


# defaults for vim - otherwise ignore
# vim:sw=4

