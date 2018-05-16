#!/usr/bin/env python

# note: due to a bug in the old pyobjc (2.5.1!!) distributed with 10.13, you
# must pip install pyobjc otherwise LSSharedFileListCreate will be undefined

from __future__ import print_function
from __future__ import absolute_import

import argparse
import sys

import CoreFoundation
import LaunchServices


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
            self._name = dname(self.ref) or "AirDrop"
        return self._name

    @property
    def path(self):
        if self._path is None:
            presolve = LaunchServices.LSSharedFileListItemResolve
            self._path = presolve(self.ref, 0, None, None)[1]
        return self._path


class lsf_list(object):
    """helper class to make LSSharedFileListCopySnapshot more pythonic"""
    def __init__(self, items):
        self.items = items

    def __iter__(self):
        snapshot = LaunchServices.LSSharedFileListCopySnapshot
        for i in snapshot(self.items, None)[0]:
            yield lsf(i)


def all_items():
    sitems = LaunchServices.LSSharedFileListCreate(
        CoreFoundation.kCFAllocatorDefault,
        LaunchServices.kLSSharedFileListFavoriteItems, None)

    snapshot = LaunchServices.LSSharedFileListCopySnapshot(sitems, None)[0]

    return [lsf(i) for i in snapshot]


def sidebar_list(*args, **opts):
    """lists the current items in the finder sidebar"""
    items = all_items()

    for i in items:
        print("(%s, %s)" % (i.name, i.path))


def sidebar_remove(item, *args, **opts):
    """remove item from the finder sidebar"""
    items = all_items()

    for i in items:
        if i.name.upper() == item.upper():
            LaunchServices.LSSharedFileListItemRemove(items, i.ref)
            return 0

    print("Couldn't find '%s', try the command 'list' to see current items"
          % item)


def main(*args, **opts):
    """our main entry point to parse args and dispatch functions"""

    actions = {c.__name__.replace('sidebar_', ''): c
               for c in [sidebar_list, sidebar_remove]}

    parser = argparse.ArgumentParser()
    parser.add_argument("action",
                        choices=actions,
                        help="action to perform on sidebar")
    parser.add_argument('item', nargs='?', default='',
                        help="item to add or remove")
    args = parser.parse_args()

    # dispatch
    ret = actions[args.action](args.item)

    CoreFoundation.CFPreferencesSynchronize(
        CoreFoundation.kCFPreferencesAnyApplication,
        CoreFoundation.kCFPreferencesCurrentUser,
        CoreFoundation.kCFPreferencesCurrentHost)
    CoreFoundation.CFPreferencesAppSynchronize("com.apple.sidebarlists")

    return ret


if __name__ == "__main__":
    sys.exit(main())
