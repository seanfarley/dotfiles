#!/usr/bin/env python

from __future__ import print_function
from __future__ import absolute_import

import argparse
import os
import sys

import Cocoa
import CoreFoundation
import LaunchServices

# fix for older pyobjc
try:
    LaunchServices.LSSharedFileListCreate
except AttributeError:
    import objc
    from Foundation import NSBundle

    bdl = NSBundle.bundleWithIdentifier_('com.apple.coreservices.SharedFileList')

    functions  = [
        ('LSSharedFileListCreate',
         '^{OpaqueLSSharedFileListRef=}^{__CFAllocator=}^{__CFString=}@'),

        ('LSSharedFileListCopySnapshot',
         '^{__CFArray=}^{OpaqueLSSharedFileListRef=}o^I'),

        ('LSSharedFileListItemCopyDisplayName',
         '^{__CFString=}^{OpaqueLSSharedFileListItemRef=}'),

        ('LSSharedFileListItemResolve',
         'i^{OpaqueLSSharedFileListItemRef=}Io^^{__CFURL=}o^{FSRef=[80C]}'),

        ('LSSharedFileListItemMove',
         'i^{OpaqueLSSharedFileListRef=}^{OpaqueLSSharedFileListItemRef=}^{OpaqueLSSharedFileListItemRef=}'),  # noqa

        ('LSSharedFileListItemRemove',
         'i^{OpaqueLSSharedFileListRef=}^{OpaqueLSSharedFileListItemRef=}'),

        ('LSSharedFileListInsertItemURL',
         '^{OpaqueLSSharedFileListItemRef=}^{OpaqueLSSharedFileListRef=}^{OpaqueLSSharedFileListItemRef=}^{__CFString=}^{OpaqueIconRef=}^{__CFURL=}^{__CFDictionary=}^{__CFArray=}'),  # noqa
    ]

    objc.loadBundleFunctions(bdl, globals(), functions)

    LaunchServices.LSSharedFileListCreate = LSSharedFileListCreate
    LaunchServices.LSSharedFileListCopySnapshot = LSSharedFileListCopySnapshot
    LaunchServices.LSSharedFileListItemCopyDisplayName = LSSharedFileListItemCopyDisplayName  # noqa
    LaunchServices.LSSharedFileListItemResolve = LSSharedFileListItemResolve
    LaunchServices.LSSharedFileListItemMove = LSSharedFileListItemMove
    LaunchServices.LSSharedFileListItemRemove = LSSharedFileListItemRemove
    LaunchServices.LSSharedFileListInsertItemURL = LSSharedFileListInsertItemURL


class lsf(object):
    """helper class to make SFLItem more pythonic"""
    def __init__(self, ref):
        self.ref = ref
        self._name = None
        self._path = None

    @property
    def name(self):
        if self._name is None:
            dname = LaunchServices.LSSharedFileListItemCopyDisplayName
            # seems that this can be empty?
            if self.path is None:
                self._name = "?"
            elif 'AirDrop' in self.path:
                self._name = "AirDrop"
            else:
                self._name = dname(self.ref)
        return self._name

    @property
    def path(self):
        if self._path is None:
            presolve = LaunchServices.LSSharedFileListItemResolve
            self._path = presolve(self.ref, 0, None, None)[1]
            if isinstance(self._path, Cocoa.NSURL):
                self._path = self._path.absoluteString()
        return self._path


class lsf_list(object):
    """helper class to make LSSharedFileListCopySnapshot more pythonic"""
    def __init__(self, items):
        self.items = items

    @property
    def _snapshot(self):
        fn = LaunchServices.LSSharedFileListCopySnapshot
        return fn(self.items, None)[0]

    def __iter__(self):
        for i in self._snapshot:
            yield lsf(i)

    def __getitem__(self, key):
        if key < 0:
            return None
        return self._snapshot[key]


def all_items():
    sitems = LaunchServices.LSSharedFileListCreate(
        CoreFoundation.kCFAllocatorDefault,
        LaunchServices.kLSSharedFileListFavoriteItems, None)

    return lsf_list(sitems)


def sidebar_list(items, *args, **opts):
    """lists the current items in the finder sidebar"""

    for i in items:
        print("%s -> %s" % (i.name, i.path))


def _rm_move(func, items, item, *args, **opts):
    """abtract same loop logic for moving and remove"""

    for i in items:
        # special case a few special names e.g. 'recent'
        if (i.name.lower() == item.lower() or
            ('recent' in item.lower() and
             'MyLibraries/myDocuments.cannedSearch' in i.path)):
            func(items.items, i.ref, *args)
            return 0

    print("Couldn't find '%s', try the command 'list' to see current items"
          % item)


def sidebar_remove(items, item, *args, **opts):
    """remove item from the finder sidebar"""
    _rm_move(LaunchServices.LSSharedFileListItemRemove, items, item)


def sidebar_move(items, item, item_after, *args, **opts):
    """remove item from the finder sidebar"""
    _rm_move(LaunchServices.LSSharedFileListItemMove, items, item, item_after)


def sidebar_insert(items, item, item_after, *args, **opts):
    """insert item after item_after in the sidebar

    Only works for physical files right now (e.g. not airdrop).

    """
    item = os.path.expanduser(os.path.expandvars(item))
    if not item.startswith("file://"):
        item = "file://" + os.path.normpath(item)

    if not os.path.exists(item[7:]):
        print("'%s' does not exist!" % item)
        return -2

    nsstr = Cocoa.NSString.alloc().initWithString_(item)
    fn = nsstr.stringByAddingPercentEscapesUsingEncoding_
    nsurl = Cocoa.NSURL.URLWithString_(fn(Cocoa.NSASCIIStringEncoding))

    new_item = LaunchServices.LSSharedFileListInsertItemURL(
        items.items, LaunchServices.kLSSharedFileListItemBeforeFirst, None,
        None, nsurl, None, None)

    new_item = lsf(new_item)

    sidebar_move(items, new_item.name, item_after)


def main(*args, **opts):
    """our main entry point to parse args and dispatch functions"""

    actions = {c.__name__.replace('sidebar_', ''): c
               for c in [sidebar_list, sidebar_remove, sidebar_move,
                         sidebar_insert]}

    parser = argparse.ArgumentParser()
    parser.add_argument("action",
                        choices=actions,
                        help="action to perform on sidebar")
    parser.add_argument('item', nargs='?', default='',
                        help="item to add or remove")
    parser.add_argument('pos', nargs='?', type=int, default=-1,
                        help="position in the list to move to")
    args = parser.parse_args()

    items = all_items()

    # dispatch
    ret = actions[args.action](items, args.item, items[args.pos])

    CoreFoundation.CFPreferencesSynchronize(
        CoreFoundation.kCFPreferencesAnyApplication,
        CoreFoundation.kCFPreferencesCurrentUser,
        CoreFoundation.kCFPreferencesCurrentHost)
    CoreFoundation.CFPreferencesAppSynchronize("com.apple.sidebarlists")

    return ret


if __name__ == "__main__":
    sys.exit(main())
