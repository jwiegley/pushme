This design document describes the next iteration of the internal architecture
for `pushme`.

The job of pushme is to synchronize **filesets**, of which there may be many.

Each fileset consists of a set of **locations** where a copy of the contents
of that fileset may reside. Locations may be filesystem directories, tarballs,
etc.

Each location is managed by a storage **schema**, such as a simple directory
tree, a git-annex repository, a ZFS filesystem, a Mac sparsebundle, etc.

Each location is also hosted by a storage **provider**, where it is possible
for a provider to host multiple files at multiple locations, and utilizing
various schema.

Communication with a provider is of two kinds: There is an administrative
channel, for making queries independent of any particular fileset or location;
and there is the transport channel, for transferring data that will update one
location to match another.

For example: I have three computers: hermes, vulcan and titan. I use Backblaze
for offsite storage, and CrashPlan for backup. I have a 10 filesets, and each
of these providers stores one copy of each fileset, with the except that
vulcan has two copies: one on the internal hard drive, and one an external
hard drive.

This counts as 5 providers, 10 filesets, and 6 locations for each fileset.

The admin channel from host to host uses SSH, while the channel within vulcan
(for transferring between the two local locations) is simply direct. The
channel to Backblaze and CrashPlan is done through their respective APIs, or
by the use of a tool that makes use of those APIs.

The transport channel from host to host is rsync, via SSH between hosts.
Copying to BackBlaze uses rclone, while copying to CrashPlan happens
automatically by way of their backup tool -- meaning that we may only query
the remote copy, and not update it directly.
