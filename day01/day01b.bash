yes "`sed s/^/last/`"|bc|perl -ne'$g{$_}++&&die$_'
