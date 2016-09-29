This is the script I use for synchronizing data between my machines (and also
to directories on the same machine, externally connected drives, and to and
between ZFS filesystems).

Each "fileset" you wish to synchronize is defined in a YAML file within
`~/.pushme/conf.d`, for example this recipe I use for synchronizing my

    Name:     'Desktop'
    Priority: 12
    Class:    'quick,main,sync'
    
    Stores:
      hermes:
        Path: /Users/johnw/Desktop
      
      maia:
        Path: /Users/johnw/Desktop
      
      vulcan:
        Path: /Users/johnw/Desktop
      
    Options:
      Rsync:
        Filters:
          - '- /annex/'

Filters may also be specified for a specific computer only:

    Name:     'Desktop'
    Priority: 12
    Class:    'quick,main,sync'
    
    Stores:
      hermes:
        Rsync:
          Path: /tank/Archives
          Filters:
            - '- /annex/'
      
      maia:
        Path: /Users/johnw/Desktop
      
      vulcan:
        Path: /Users/johnw/Desktop

There are three backends supported for file transfer `Rsync` (the default, if
none is specified), `Zfs`, and `Annex`.  When two backends mismatch for a
given machine, `Rsync` is used, otherwise the most optimal method for
synchronizing that particular fileset type is attempted.

Note that recently I have only been using the `Rsync` method, so the other
backends are not well tested, and should not be used except on trial data at
this time. If you wish to help support them, I am available for assistance.

Pushme is invoked as follows (`--from` can be omitted, if `hostname` returns
the same string):

    pushme --from thisMachine thatMachine

This command will synchronize every fileset that contains a backend definition
for both `thisMachine` and `thatMachine`. Here is example output from such a
command, assuming two filesets `home` and `local`:

    foo $ pushme thatMachine
    15:18:44 - [NOTICE] Synchronizing ThisMachine -> ThatMachine
    15:18:44 - [NOTICE] Sending ThisMachine/home → ThatMachine
    15:18:52 - [NOTICE] Sent 151.0M in 131 files (out of 1.37G in 12,418)
    15:20:26 - [NOTICE] Sending ThisMachine/local → ThatMachine
    15:21:02 - [NOTICE] Sent 0b in 0 files (out of 6.45G in 207,453)

Some common options include:

    $ pushme -c quick bar       # only sync filesets with the "quick" class
    $ pushme -f home bar        # only sync the "home" fileset
    $ pushme -n bar             # don't do anything, just scan
    $ pushme -N bar             # don't even scan
    $ pushme -v bar             # show commands being executed
    $ pushme -D bar             # show more info than you want to see
