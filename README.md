[pushme](https://github.com/jwiegley/pushme) is a wrapper around
[rsync](https://en.wikipedia.org/wiki/Rsync) allowing declarative filesets to
be transferred between machines.

Filesets are declared, one per file, in the directory
`~/.config/pushme/filesets`. An exhaustive set of options are given in the
following `src.yaml` example:

```
# The `src` fileset is transferred when I use the following command:
#   pushme hera@24 clio@10 athena@8 tank@1

# Filesets can be specifically named using -f name1,name2,…
Name:     'src'
# Filesets are transmitted in priority order
Priority: 50
# Classes allow transferring subsets using -c class1,class2,…
Classes:
  - 'small'

# Define on which machines the fileset is located, where, and if custom
# rsync options apply
Stores:
  hera:
    # The remote pathname on `hera` where this fileset resides
    Path: /Users/johnw/src

    # Filters passed to rsync whenever transferring TO `hera`, and also
    # if transferring FROM `hera` and the target does not specify its own.
    Filters: |
      - *~
    # If NoBasicOptions is set, then `-a` is not passed to `rsync`
    NoBasicOptions: false
    # If NoDelete is set, then `--delete` is not passed to `rsync`
    NoDelete: true
    # If PreserveAttrs is set, then `-AXUNHE` is passed to `rsync`. This is
    # not the default.
    PreserveAttrs: true
    # If ProtectTopLevel is set, filters are dynamically created that exclude
    # every remote entry not existing on the source. There is no corresponding
    # flag offered by rsync, so this is implemented in pushme.
    ProtectTopLevel: false
    # Additional `rsync` options added, in addition to any others.
    Options:
      - "--delete-after"
    # Only transfer here if we are transferring FROM the given machines.
    ReceiveFrom:
      - clio
    # Is this fileset active on this machine? If not, never transfer here.
    Active: true

  clio:
    Path: /Users/johnw/src
    ReceiveFrom:
      - hera

  athena:
    Path: /Users/johnw/src
    ReceiveFrom:
      - hera
      - clio

  # `tank` is the same machine as `athena`, it just uses a different set of
  # platter-based directorys for long-term, ZFS archival storage. By using a
  # separate ssh hostname to refer to `athena` this way, I gain additional
  # flexibility as to which filesets get transferred and how much parallelism
  # is used when receiving files (to reduce fragmentation)
  tank:
    Path: /Volumes/tank/src
    ReceiveFrom:
      - hera
      - clio

# In addition to specifying specific options for given targets above, you can
# also specify options here that apply to all targets. Note that each setting
# here may be overridden by target specific settings.
Common:
  Filters: |
    # Include foo, but only some of its children
    + /foo/
    + /foo/bar
    - /foo/*
    - /foo/*/
    - /foo/.*
    - /foo/.*/
    # Include everything but for these extensions, appearing anywhere
    - *.agdai
    - *.d
    - *.glob
    - *.hi
```

Once you have defined a group of filesets, you may transfer all filesets that
apply from one machine to any others using the basic command:

    pushme SOURCE TARGETS…
    
For example, I have a desktop `hera`, a laptop `clio`, a server `athena` and a
ZFS drive on `athena` that I reference using the ssh hostname `tank`. Thus I
might use any of the following commands:

    pushme hera clio                # update the laptop
    pushme hera clio athena         # update the laptop and server
    pushme hera clio athena tank    # update both and archival store

If a machine has multiple cores, you can take advantage of parallelism by
specifying how many simultaneous transfer jobs you’d like to support either
from or to a particular machine:

    pushme hera@24 clio@14 athena@10 tank@1

Filesets are always transferred in priority order, independent of how many
times a particular fileset may be “in flight” at a given moment when
transferring to multiple machines. This should ensure that the most important
data is always completed first, before transferring other filesets.

In addition to setting rsync options per-target or common to all targets, you
may also defined some global options using `~/.config/pushme/config.yaml`:

```
# If DryRun is true, no changes will be made to any target fileset
DryRun: false
# If Filesets is defined here, only these filesets are eligible for transfer
Filesets:
  - 'foo'
# If Classes is defined here, only matching filesets are eligible for transfer
Classes:
  - 'foo'
# If SIUnits is true, report in gigabytes, for example, instead of gibibytes
SIUnits: true
# If Verbose is true, present a great amount of detail
Verbose: false
# GlobalOptions are like having a Common block in every fileset, except they
# are overriden if fileset Common options are specified, or for any target
GlobalOptions:
  PreserveAttrs: true
  Options:
    - "--include-from=/Users/johnw/.config/ignore.lst"
```

Finally, options may also be specified using the command-line, but these have
the lowest priority in case the same option has also been defined in the
config file or the fileset declaration:

```
--config ~/.config/pushme
-n/--dry-run
-f/--filesets foo,bar,…
-c/--classes foo,bar,…
-s/--si-units
-v/--verbose
--rsync-filters "- foo\n- bar\n"
--rsync-no-basic-options
--rsync-no-delete
--rsync-preserve-attrs
--rsync-protect-top-level
--rsync-options "opt1 opt2 opt3"
```

For an option like `--rsync-options`, the priority is to use options specified:

1. For a specific fileset target;
2. In the `Common` settings of a fileset;
3. In the `GlobalOptions` settings of `config.yaml`;
4. On the command line.

Each of these completely overrides any options specified by a later entry, so
if options have been given for a fileset target, any other options defined in
other places will be ignored when transferring to that target.
