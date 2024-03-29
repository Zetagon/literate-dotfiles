#!/usr/bin/env bash
set -euo pipefail

# Setting this, so the repo does not need to be given on the commandline:
# The variable needs to be set when calling this script
# export BORG_REPO=/media/leo/Backup/borg-backup

# See the section "Passphrase notes" for more infos.
export BORG_PASSCOMMAND='pass show usb-backup'

# some helpers and error handling:
info() { printf "\n%s %s\n\n" "$( date )" "$*" >&2; }
trap 'echo $( date ) Backup interrupted >&2; exit 2' INT TERM

info "Starting backup"

# Backup the most important directories into an archive named after
# the machine this script is currently running on:

borg create                         \
    --verbose                       \
    --filter AME                    \
    --list                          \
    --stats                         \
    --show-rc                       \
    --compression zstd,22               \
    --exclude-caches                \
    --exclude '*/.git/*'    \
    --exclude '*/.stversions/*'    \
    --exclude '/var/tmp/*'          \
    --exclude '/home/leo/org/.dropbox-dist/*'                     \
    --exclude '/home/leo/org/.dropbox/*'                     \
                                    \
    ::'{hostname}-{now}'            \
    ~/Documents/notes/              \
    ~/org/                           \
    ~/Documents/accounting/                           \
    ~/Documents/Litteratur_Analys/

backup_exit=$?

info "Pruning repository"

# Use the `prune` subcommand to maintain 7 daily, 4 weekly and 6 monthly
# archives of THIS machine. The '{hostname}-' prefix is very important to
# limit prune's operation to this machine's archives and not apply to
# other machines' archives also:

borg prune                          \
    --list                          \
    --prefix '{hostname}-'          \
    --show-rc                       \
    --keep-daily    7               \
    --keep-weekly   4               \
    --keep-monthly  6               \

prune_exit=$?

# use highest exit code as global exit code
global_exit=$(( backup_exit > prune_exit ? backup_exit : prune_exit ))

if [ ${global_exit} -eq 0 ]; then
    info "Backup and Prune finished successfully"
elif [ ${global_exit} -eq 1 ]; then
    info "Backup and/or Prune finished with warnings"
else
    info "Backup and/or Prune finished with errors"
fi

exit ${global_exit}
