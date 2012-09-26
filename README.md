This is the script I use for synchronizing data between my machines (and also
to directories on the same machine, externally connected drives, and to and
between ZFS filesystems).

Here is an example set of files for synchronizing my home directory and
`/usr/local` via rsync between two machines: `foo` and `bar`.  First,
`~/.pushme/stores.yml`:

    - 'Targets':
      - - 'bar'
        - - 'home'
          - 'local'
      'HostRe': '[Ff]oo'
      'Name': 'Foo'
      'UserName': 'johnw'
      'SelfRe': '[Ff]oo'
      'ZfsPool': null
      'ZfsPath': null
    - 'Targets':
      - - 'foo'
        - - 'home'
          - 'local'
      'HostRe': '[Bb]ar'
      'Name': 'Bar'
      'UserName': 'johnw'
      'SelfRe': '[Bb]ar'
      'ZfsPool': null
      'ZfsPath': null

Then, `~/.pushme/filesets.yml`:

    - 'Priority': 60
      'Name': 'home'
      'Class': 'quick,main'
      'ReportMissing': true
    - 'Priority': 80
      'Name': 'local'
      'Class': 'system,main'
      'ReportMissing': false

And finally, `~/.pushme/containers.yml`:

    - 'Store': 'foo,bar'
      'Recurse': false
      'Fileset': 'home'
      'PoolPath': null
      'LastRev': null
      'LastSync': null
      'Path': '~/'
    - 'Store': 'foo,bar'
      'Recurse': false
      'Fileset': 'local'
      'PoolPath': null
      'LastRev': null
      'LastSync': null
      'Path': '/usr/local/'

Now I can run the following command:

    foo $ pushme bar
    15:18:44 - [NOTICE] Synchronizing Foo -> Bar
    15:18:44 - [NOTICE] Sending Foo/home → Bar
    15:18:52 - [NOTICE] Sent 151.0M in 131 files (out of 1.37G in 12,418)
    15:20:26 - [NOTICE] Sending Foo/local → Bar
    15:21:02 - [NOTICE] Sent 0b in 0 files (out of 6.45G in 207,453)

If you are wondering about the complexity of keeping filesets separate from
containers, it's because I use pushme for several different scenarios:
rsync'ing between machines, rsync'ing into ZFS filesystems, and sending ZFS
filesystems as snapshot stream between servers.  Pushme is able to handle all
of these scenarios, but doing so required a bit more abstraction than a simple
rsync-only tool would have needed.

Some common options include:

    $ pushme -c quick bar       # only sync filesets with the "quick" class
    $ pushme -f home bar        # only sync the "home" fileset
    $ pushme -n bar             # don't do anything, just scan
    $ pushme -N bar             # don't even scan
    $ pushme -v bar             # show commands being executed
    $ pushme -D bar             # show more info than you want to see
