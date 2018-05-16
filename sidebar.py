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
        snapshot = LaunchServices.LSSharedFileListCopySnapshot
        self._snapshot = snapshot(items, None)[0]
        self.items = items

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


def sidebar_list(*args, **opts):
    """lists the current items in the finder sidebar"""
    items = all_items()

    for i in items:
        print("(%s, %s)" % (i.name, i.path))


def _rm_move(item, func, *args, **opts):
    items = all_items()
    pargs = []

    pos = opts.get('pos')
    if opts and pos is not None:
        pargs.append(items[pos])

    for i in items:
        # special case a few special names e.g. 'recent'
        if (i.name.lower() == item.lower() or
            ('recent' in item.lower() and
             'MyLibraries/myDocuments.cannedSearch' in
             i.path.absoluteString())):
            func(items.items, i.ref, *pargs)
            return 0

    print("Couldn't find '%s', try the command 'list' to see current items"
          % item)


def sidebar_remove(item, *args, **opts):
    """remove item from the finder sidebar"""
    return _rm_move(item, LaunchServices.LSSharedFileListItemRemove)


def sidebar_move(item, pos, *args, **opts):
    """remove item from the finder sidebar"""
    return _rm_move(item, LaunchServices.LSSharedFileListItemMove,
                    pos=pos)


def main(*args, **opts):
    """our main entry point to parse args and dispatch functions"""

    actions = {c.__name__.replace('sidebar_', ''): c
               for c in [sidebar_list, sidebar_remove, sidebar_move]}

    parser = argparse.ArgumentParser()
    parser.add_argument("action",
                        choices=actions,
                        help="action to perform on sidebar")
    parser.add_argument('item', nargs='?', default='',
                        help="item to add or remove")
    parser.add_argument('pos', nargs='?', type=int, default=0,
                        help="position in the list to move to")
    args = parser.parse_args()

    # dispatch
    ret = actions[args.action](args.item, args.pos)

    CoreFoundation.CFPreferencesSynchronize(
        CoreFoundation.kCFPreferencesAnyApplication,
        CoreFoundation.kCFPreferencesCurrentUser,
        CoreFoundation.kCFPreferencesCurrentHost)
    CoreFoundation.CFPreferencesAppSynchronize("com.apple.sidebarlists")

    return ret


if __name__ == "__main__":
    sys.exit(main())
