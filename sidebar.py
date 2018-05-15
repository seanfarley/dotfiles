#!/usr/bin/env python

# note: due to a bug in the old pyobjc (2.5.1!!) distributed with 10.13, you
# must pip install pyobjc otherwise LSSharedFileListCreate will be undefined

from __future__ import print_function
from __future__ import absolute_import

import argparse
import sys

import CoreFoundation
import LaunchServices


def sidebar_list(*args, **opts):
    """lists the current items in the finder sidebar"""
    items = LaunchServices.LSSharedFileListCreate(
        CoreFoundation.kCFAllocatorDefault,
        LaunchServices.kLSSharedFileListFavoriteItems, None)

    snapshot = LaunchServices.LSSharedFileListCopySnapshot(items, None)[0]
    resolv = LaunchServices.LSSharedFileListItemResolve

    for i in snapshot:
        name = LaunchServices.LSSharedFileListItemCopyDisplayName(i)
        path = ""
        if name not in ("AirDrop", "All My Files", "iCloud"):
            path = resolv(i, 0, None, None)[1]

        print("(%s, %s)" % (name, path))


def main(*args, **opts):
    """our main entry point to parse args and dispatch functions"""

    actions = {c.__name__.replace('sidebar_', ''): c
               for c in [sidebar_list]}

    parser = argparse.ArgumentParser()
    parser.add_argument("action",
                        choices=actions,
                        help="action to perform on sidebar")

    args = parser.parse_args()

    # dispatch
    actions[args.action]()

    CoreFoundation.CFPreferencesSynchronize(
        CoreFoundation.kCFPreferencesAnyApplication,
        CoreFoundation.kCFPreferencesCurrentUser,
        CoreFoundation.kCFPreferencesCurrentHost)
    CoreFoundation.CFPreferencesAppSynchronize("com.apple.sidebarlists")

    return 0


if __name__ == "__main__":
    sys.exit(main())
