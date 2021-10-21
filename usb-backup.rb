#!/usr/bin/env ruby
if `hostname` == "sakura\n"
  `BORG_REPO=/run/media/leo/Backup/borg-backup ~/.doom.d/borg-backupscript.sh`
else
  if `hostname` == "hako\n"
    `BORG_REPO=/media/leo/Backup/borg-backup ~/.doom.d/borg-backupscript.sh`
  end
end
