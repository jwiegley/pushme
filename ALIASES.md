# Alias Feature Documentation

## Overview

The alias feature allows you to define named shortcuts for hosts with custom configuration including parallelism levels and multiple interpolatable variables. This makes it easier to manage multiple hosts and enables flexible path interpolation using any number of named variables.

## Configuration

Aliases are defined in `~/.config/pushme/config.yaml` under the `Aliases` section:

```yaml
Aliases:
  # Map 'tank' to actual host with multiple variables
  tank:
    Host: vulcan
    MaxJobs: 8
    Variables:
      prefix: /Volumes/tank
      home: /Users/johnw
      data: /Volumes/tank/data
      backup: /Volumes/tank/backups

  # You can define an alias with the same name as the hostname
  hera:
    Host: hera
    MaxJobs: 10
    Variables:
      home: /Users/johnw
      src: /Users/johnw/src
      config: /Users/johnw/.config

  # Simple alias without variables
  dev:
    Host: development-server
    MaxJobs: 2

  # Linux server with different path structure
  linux-prod:
    Host: prod.example.com
    MaxJobs: 12
    Variables:
      home: /home/user
      var: /var
      opt: /opt/apps
      logs: /var/log
```

## Alias Fields

- **Host** (required): The actual hostname or IP to connect to. Can include `@N` notation for jobs (e.g., `vulcan@8`)
- **MaxJobs** (optional): Number of parallel transfers for this host. Defaults to 1 if not specified
- **Variables** (optional): Map of variable names to values for path interpolation. Any number of variables can be defined

## Usage

### Basic Usage

Once aliases are defined, use them exactly like regular hostnames:

```bash
# Use 'tank' alias (maps to vulcan@8)
pushme hera tank

# Use multiple aliases
pushme hera tank clio

# Mix aliases and regular hostnames
pushme hera athena@12
```

### Fileset Matching

Filesets match against the **alias name** (or hostname if no alias), not the actual hostname:

```yaml
# ~/.config/pushme/filesets/src.yaml
Name: 'source-code'
Stores:
  tank:              # Matches the 'tank' alias
    Path: /vol/src

  hera:              # Matches the 'hera' alias (or direct hostname)
    Path: /Users/johnw/src
```

### Path Interpolation with Variables

Use `$varname` in fileset paths to reference any variable defined in the alias:

```yaml
# Config with multiple variables
Aliases:
  dev:
    Host: dev-server
    MaxJobs: 4
    Variables:
      home: /home/user
      config: /etc/app
      data: /var/data
      logs: /var/log/app

  prod:
    Host: prod-server
    MaxJobs: 8
    Variables:
      home: /home/prod
      config: /opt/app/config
      data: /mnt/data
      logs: /mnt/logs

# Fileset using multiple variables
Name: 'application'
Stores:
  dev:
    Path: $data/app           # Expands to /var/data/app
  prod:
    Path: $data/app           # Expands to /mnt/data/app

Name: 'logs'
Stores:
  dev:
    Path: $logs/daily         # Expands to /var/log/app/daily
  prod:
    Path: $logs/daily         # Expands to /mnt/logs/daily

Name: 'config'
Stores:
  dev:
    Path: $config             # Expands to /etc/app
  prod:
    Path: $config             # Expands to /opt/app/config
```

**Variable Syntax Rules:**
- Variable names must match pattern: `$[a-zA-Z_][a-zA-Z0-9_]*`
- Valid: `$prefix`, `$home`, `$my_var`, `$DATA_DIR`, `$path2`
- Invalid: `$2var` (starts with number), `$my-var` (contains hyphen), `$` (no name)

**Error Handling:**
If a path contains a variable that isn't defined, pushme will error with a helpful message:
```
Error: Path contains undefined variable $missing in path: $missing/data
Available variables: ["home", "config", "data", "logs"]
```

**Multiple Variables in One Path:**
```yaml
Path: $home/$config/settings.yml    # Both variables interpolated
# With dev alias: /home/user//etc/app/settings.yml
```

### Parallelism Control

The MaxJobs setting controls how many transfers can run in parallel:

```yaml
Aliases:
  fast:
    Host: high-bandwidth-server
    MaxJobs: 16        # Up to 16 concurrent transfers

  slow:
    Host: limited-server
    MaxJobs: 2         # Only 2 concurrent transfers
```

This is equivalent to the old `hostname@N` notation but more explicit and manageable.

### Local Transfers

When the source and destination resolve to the **same actual hostname**, pushme performs a local transfer:

```yaml
Aliases:
  local-work:
    Host: localhost
    Prefix: /home/user/work

  local-backup:
    Host: localhost
    Prefix: /backup
```

```bash
# This is detected as a local transfer (both resolve to localhost)
pushme local-work local-backup
```

## Advanced Examples

### Environment-Based Aliases with Multiple Variables

```yaml
Aliases:
  # Development environment
  dev-db:
    Host: dev-database.local
    MaxJobs: 2
    Variables:
      data: /data/dev
      logs: /var/log/dev/db
      backup: /backup/dev/db

  dev-app:
    Host: dev-app.local
    MaxJobs: 4
    Variables:
      app: /opt/dev/app
      config: /etc/dev/app
      logs: /var/log/dev/app
      static: /var/www/dev

  # Production environment
  prod-db:
    Host: prod-database.company.com
    MaxJobs: 8
    Variables:
      data: /data/prod
      logs: /var/log/prod/db
      backup: /mnt/backup/prod/db

  prod-app:
    Host: prod-app.company.com
    MaxJobs: 12
    Variables:
      app: /opt/prod/app
      config: /etc/prod/app
      logs: /var/log/prod/app
      static: /var/www/prod
```

### Cross-Platform Path Mapping

Handle macOS and Linux path differences:

```yaml
Aliases:
  # macOS workstation
  mac-work:
    Host: macbook.local
    MaxJobs: 8
    Variables:
      home: /Users/johnw
      config: /Users/johnw/.config
      documents: /Users/johnw/Documents
      volumes: /Volumes

  # Linux server
  linux-prod:
    Host: server.company.com
    MaxJobs: 16
    Variables:
      home: /home/johnw
      config: /home/johnw/.config
      documents: /home/johnw/Documents
      mnt: /mnt

# Fileset that works on both platforms
Name: 'dotfiles'
Stores:
  mac-work:
    Path: $config/dotfiles
  linux-prod:
    Path: $config/dotfiles
```

### Multi-Site Setup

```yaml
Aliases:
  # West coast datacenter
  west:
    Host: sync.west.company.com
    MaxJobs: 24

  # East coast datacenter
  east:
    Host: sync.east.company.com
    MaxJobs: 24

  # Cloud backup
  cloud:
    Host: backup.s3.amazonaws.com
    MaxJobs: 8
```

## Backward Compatibility

The alias feature is fully backward compatible:

- Hosts can still be specified as `hostname` or `hostname@N`
- Filesets without corresponding aliases work as before
- Mixing aliased and non-aliased hosts in a single command works
- All existing configurations continue to work unchanged
- **Legacy `Prefix` field**: Old configs using `Prefix: /path` are automatically converted to `Variables: {prefix: /path}`
  ```yaml
  # Old format (still works)
  Aliases:
    tank:
      Host: vulcan
      Prefix: /tank

  # Automatically treated as:
  Variables:
    prefix: /tank

  # Can use $prefix in paths
  Path: $prefix/data  # Works with both formats
  ```

## Migration Guide

### From hostname@N to Aliases

To migrate existing configurations to use aliases:

1. **Identify your common hosts**:
   ```bash
   # Old way
   pushme vulcan@8 athena@12 zeus@6
   ```

2. **Create aliases in config.yaml**:
   ```yaml
   Aliases:
     vulcan:
       Host: vulcan
       MaxJobs: 8
     athena:
       Host: athena
       MaxJobs: 12
     zeus:
       Host: zeus
       MaxJobs: 6
   ```

3. **Simplify your commands**:
   ```bash
   # New way (equivalent)
   pushme vulcan athena zeus
   ```

### From Prefix to Variables

To migrate from the old `Prefix` field to `Variables`:

1. **Current config with Prefix**:
   ```yaml
   Aliases:
     tank:
       Host: vulcan
       Prefix: /Volumes/tank
   ```

2. **Add more variables** (Prefix still works, but Variables is more flexible):
   ```yaml
   Aliases:
     tank:
       Host: vulcan
       Variables:
         prefix: /Volumes/tank      # Keep old prefix variable
         home: /Users/johnw         # Add new variables
         data: /Volumes/tank/data
         backup: /Volumes/tank/backup
   ```

3. **Update filesets to use specific variables**:
   ```yaml
   # Before
   Path: $prefix/backups

   # After (more specific)
   Path: $backup              # Or $data, $home, etc.
   ```

4. **Update filesets to use alias names**:
   - No changes needed if alias name matches hostname
   - Otherwise update fileset Store names to match alias names

## Troubleshooting

### Path contains undefined variable

**Error**: "Path contains undefined variable $varname in path: /some/$varname/path"
```
Available variables: ["home", "config", "data"]
```

**Solution**: Either:
- Add the missing variable to the alias's `Variables` section:
  ```yaml
  Variables:
    varname: /some/path
  ```
- Remove or rename the variable reference in the fileset path
- Check for typos in variable names (they're case-sensitive)

### Fileset store not found

**Error**: No matching store in fileset for alias

**Solution**: Ensure fileset Store names match your alias names (not the underlying hostnames)

### MaxJobs not being respected

**Issue**: Transfers seem slower than expected

**Check**:
- Verify MaxJobs is set in the alias
- Both source and destination hosts need sufficient MaxJobs
- Network bandwidth may be the bottleneck, not parallelism
