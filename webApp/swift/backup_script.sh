#!/bin/bash
#TODO:
#Transfer backups to remote server, also do a git push for the constitutions
while true; do



#GIT PUSH to backup server:
#[webapproot]/swift/src/main/webapp/constitutions/.git

# directories to backup
ORIGINALDIRS="persist src/main/webapp/constitutions H2_database_backup.zip"
BACKUPDIR=`pwd`/backups/`date +"%Y.%m.%d.%H.%M.%S"`

#H2 database backup:
PAADJE="/home/$USER/.m2/repository/com/h2database/h2/1.2.138/h2-1.2.138.jar"

#java backup code:
java -cp $PAADJE org.h2.tools.Shell -url "jdbc:h2:lift_proto.db;AUTO_SERVER=TRUE" -sql "BACKUP TO 'H2_database_backup.zip'" >/dev/null

# other possibilities: $HOME_/.mozilla-thunderbird/9qao2p0n.default"

# excludes file - this contains a wildcard pattern per line of files to exclude
# EXCLUDES=...

# the name of the backup machine
#BSERVER=...

# the name of the backup directory
#if [[ ! -f "$DRIVEROOT/CGBACKUPHDD" ]]; then
#   echo "CG backup drive not mounted on $DRIVEROOT, please do this first."
#   exit
#fi
#BACKUPDIR=...
if [[ ! -d "$BACKUPDIR" ]]; then mkdir -p "$BACKUPDIR"; fi
# EXPIRATIONINTERVAL="" # Read this as: "all backups created longer than $EXPIRATIONINTERVAL will be deleted". Use the format of the unix find command, as explained. If you don't want experiation, dont set this variable

# find -maxdepth 1 \! -newermt "`date -d "18 minute ago"`"

# your password on the backup server
# export RSYNC_PASSWORD=XXXXXX

########################################################################

if [[ ! -d "$BACKUPDIR/current" ]]; then mkdir "$BACKUPDIR/current"; fi

BACKUPARCHIVEDIR=$BACKUPDIR/`date +"%Y.%m.%d.%H.%M.%S"`
echo $BACKUPARCHIVEDIR  >/dev/null
# `date +%A`
# original: OPTS="--force --ignore-errors --delete-excluded --exclude-from=$EXCLUDES 
#      --delete --backup --backup-dir=/$BACKUPDIR -a"

OPTS="--force --ignore-errors 
      --delete --backup --backup-dir=$BACKUPARCHIVEDIR -a"

export PATH=$PATH:/bin:/usr/bin:/usr/local/bin

# the following line clears the last weeks incremental directory
#OLDESTDIR=`ls -1t|tail -1`
# Chide@&y2010.03.24.18:28:23&: I think the following is a (not so elegant) work-around to delete all files in a certain directory with rsync
#[ -d $HOME_/emptydir ] || mkdir $HOME_/emptydir
#rsync --delete -a $HOME_/emptydir/ $OLDESTDIR
#rmdir $HOME_/emptydir

# Chide&y2010.03.24.18:36:26&, I changed this to:
if [[ $EXPIRATIONINTERVAL != "" ]]; then
find $BACKUPDIR -maxdepth 1 \! -newermt "`date -d "$EXPIRATIONINTERVAL"`" \! -iname \*current\* \! -path "$BACKUPDIR" -exec rm -r {} \;
fi
# now the actual transfer
for ordir in $ORIGINALDIRS
do
   rsync $OPTS $ordir $BACKUPDIR/current
done

#remove backup.zip because it has been backuped.
rm H2_database_backup.zip

sleep 10m

done
