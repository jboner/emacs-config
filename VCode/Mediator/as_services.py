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
# (C) 2001, National Research Council of Canada
#
##############################################################################

"""Support services for [AppState] subclasses.

This module defines a series of services that can be used by various
[AppState] subclasses to implement concrete methods.

The concrete behaviour is factored into services as opposed to being
implemented directly as methods of [AppState] subclasses. This is to
allow more flexible mixin of implementation of behaviours than can be
supported through subclassing.

For example, class AppStateX may need to implement some methods like
class AppStateY, but other methods like class AppStateZ. With
subclassing, AppStateX would have to derive from both AppStateY and
AppStateZ, which means that either AppStateY would have to derive from
AppStateZ or vice-versa (can't use multiple inheritance to make
AppStateX inherit from both AppStateY and AppStateZ because that would
result in method name clashes). But maybe AppStateY and AppStateZ need
to be on different branches of the inheritance hierarchy.

By implementing variants of a given methods as variants of a given
service, we make it possible for two classes to use the same variant
of a method even if they are not on the same branch of the inheritance
hierarchy. Without such an approach, the code implementing that
variant would have to be cloned and copied to the two classes.


..[AppState] file:///./AppState.AppState.html"""

import Object
from debug import trace

class AS_Service(Object.OwnerObject):
    """Support service for AppState classes.

    Class for defining some sort of service that can be used by various
    [AppState] subclasses to implement concrete methods.

    For more details on *AS_Service*'s reason for existence,
    consult documentation of the [as_services] module.
    
    **INSTANCE ATTRIBUTES**
    
    [AppState] *app* -- The application for which we are providing the service.

    CLASS ATTRIBUTES**
    
    *none* -- 


    ..[AppState] file:///./AppState.AppState.html
    ..[as_services] file:///./as_services.as_services.html"""
    
    def __init__(self, app=None, **args_super):
        self.deep_construct(AS_Service, 
                            {'app': app}, 
                            args_super, 
                            {})
        self.name_parent('app')


class AS_ServiceBreadcrumbs(AS_Service):
    """Implements a VoiceCode level breadcrumbs service.

    Breadcrumbs provide the equivalent of the *Back* and *Forward*
    buttons on a web browser.

    This particular version of the breadcrumb service maintains the
    breadcrumbs at the VoiceCode level, so that it can be supported
    even with editors that do not have bookmarking capabilities.

    [AppState] instances designed for editors that do have bookmarking
    capabitilies may choose to implement their own breadcrumb service
    based by having the editor keep track of the breadcrumbs.
    
    **INSTANCE ATTRIBUTES**

    *[(STR, INT)]* breadcrumbs = []-- stack of breadcrumbs. Each entry of
     the stack is a couple where the first entry is the name of the
     source buffer and the second is the position in that buffer where
     the crumb was dropped.
        
    CLASS ATTRIBUTES**
    
    *none* -- 

    ..[AppState] file:///./AppState.AppState.html"""
    
    def __init__(self, breadcrumbs=[], **args_super):
        self.deep_construct(AS_ServiceBreadcrumbs, 
                            {'breadcrumbs': breadcrumbs}, 
                            args_super, 
                            {})

    def drop_breadcrumb(self, buff_name=None, pos=None):

        """Drops a breadcrumb

        *INT pos* is the position where to drop the crumb. *STR
         buff_name* is the name of the source buffer.
        
        If *pos* not specified, drop breadcrumb at cursor position.

        If *buff* not specified either, drop breadcrumb in current buffer
        """
        buff = self.app.find_buff(buff_name)
        
#        file_name = buff.file_name()
        if not pos: pos = buff.cur_pos()
        self.breadcrumbs = self.breadcrumbs + [[buff_name, pos]]


    def pop_breadcrumbs(self, num=1, gothere=1):
        """Pops breadcrumbs from the breadcrumbs stack

        *INT num* is the number of crumbs to pop. If None, then pop 1 crumb.

        if *BOOL gothere* is true, then move cursor to the last popped
        breadcrumb.
        """

        trace('as_services.SBServiceBreadcrumbs.pop_breadcrumb', 'num=%s, gothere=%s' % (num, gothere))
#        print '-- as_services.SBServiceBreadcrumbs.pop_breadcrumb:  self.breadcrumbs=%s' % self.breadcrumbs       
        stacklen = len(self.breadcrumbs)
        lastbuff, lastpos = self.breadcrumbs[stacklen - num]

        trace('SBServiceBreadcrumbs.pop_breadcrumb', 
            'lastbuff, lastpos = %s, %d' % (lastbuff, lastpos))
        self.breadcrumbs = self.breadcrumbs[:stacklen - num]
        if gothere:
            self.app.goto(lastpos, buff_name=lastbuff)


class AS_ServiceLangName(AS_Service):
    
    """Provides services for determining the programming language of a
    particular buffer.

    This service is implemented completely at the VoiceCode level so that it 
    can be used by [AppState] classes for editors that are not language aware.
    
    **INSTANCE ATTRIBUTES**
    
    *none*-- 
    
    CLASS ATTRIBUTES**
    
    *none* -- 
    """
    
    def __init__(self, **args_super):
        self.deep_construct(AS_ServiceLanguages, 
                            {}, 
                            args_super, 
                            {})
        
    def active_language(self):
        """Returns name of active programming language.

        If no active programming language, then returns *None*.
        
        **INPUTS**
        
        *none* -- 
        
        **OUTPUTS**
        
        *STR* language -- Name of active programming language (*None*
        if no programming language is active).
        """
        
        language = None
        if self.app.curr_buffer() != None:
            language = self.app.curr_buffer().language
        return language


