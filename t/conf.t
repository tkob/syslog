# Setup

    - CM.make "syslogd.cm";
    ...
    val it = true : bool

    - val inputLine = TextIO.StreamIO.inputLine;
    ...
    - val lines = TextIO.getInstream o TextIO.openString;
    ...

# All crit or severer messages, except for kern

    - val conf = SyslogConf.load inputLine (lines "*.crit;kern.none /var/adm/critical");
    ...

    - SyslogConf.run conf (Syslog.Facility.User, Syslog.Severity.Crit);
    val it = [File "/var/adm/critical"] : SyslogConf.action list

    - SyslogConf.run conf (Syslog.Facility.User, Syslog.Severity.Alert);
    val it = [File "/var/adm/critical"] : SyslogConf.action list

    - SyslogConf.run conf (Syslog.Facility.User, Syslog.Severity.Err);
    val it = [] : SyslogConf.action list

    - SyslogConf.run conf (Syslog.Facility.Kern, Syslog.Severity.Crit);
    val it = [] : SyslogConf.action list

    - SyslogConf.run conf (Syslog.Facility.Kern, Syslog.Severity.Alert);
    val it = [] : SyslogConf.action list

    - SyslogConf.run conf (Syslog.Facility.Kern, Syslog.Severity.Err);
    val it = [] : SyslogConf.action list

# All kern messages

    - val conf = SyslogConf.load inputLine (lines "kern.* /var/adm/kern");
    ...

    - SyslogConf.run conf (Syslog.Facility.Kern, Syslog.Severity.Emerg);
    val it = [File "/var/adm/kern"] : SyslogConf.action list

    - SyslogConf.run conf (Syslog.Facility.Kern, Syslog.Severity.Alert);
    val it = [File "/var/adm/kern"] : SyslogConf.action list

    - SyslogConf.run conf (Syslog.Facility.Kern, Syslog.Severity.Crit);
    val it = [File "/var/adm/kern"] : SyslogConf.action list

    - SyslogConf.run conf (Syslog.Facility.Kern, Syslog.Severity.Err);
    val it = [File "/var/adm/kern"] : SyslogConf.action list

    - SyslogConf.run conf (Syslog.Facility.Kern, Syslog.Severity.Warning);
    val it = [File "/var/adm/kern"] : SyslogConf.action list

    - SyslogConf.run conf (Syslog.Facility.Kern, Syslog.Severity.Notice);
    val it = [File "/var/adm/kern"] : SyslogConf.action list

    - SyslogConf.run conf (Syslog.Facility.Kern, Syslog.Severity.Info);
    val it = [File "/var/adm/kern"] : SyslogConf.action list

    - SyslogConf.run conf (Syslog.Facility.Kern, Syslog.Severity.Debug);
    val it = [File "/var/adm/kern"] : SyslogConf.action list

    - SyslogConf.run conf (Syslog.Facility.User, Syslog.Severity.Emerg);
    val it = [] : SyslogConf.action list

# mail.info and news.info

    - val conf = SyslogConf.load inputLine (lines "mail,news.info /var/adm/info");
    ...

    - SyslogConf.run conf (Syslog.Facility.Mail, Syslog.Severity.Info);
    val it = [File "/var/adm/info"] : SyslogConf.action list

    - SyslogConf.run conf (Syslog.Facility.New, Syslog.Severity.Info);
    val it = [File "/var/adm/info"] : SyslogConf.action list

    - SyslogConf.run conf (Syslog.Facility.User, Syslog.Severity.Info);
    val it = [] : SyslogConf.action list

# Multiple rules

    - val conf = SyslogConf.load inputLine (lines "kern.* /var/adm/kernel\nkern.crit /dev/console\n");
    ...

    - SyslogConf.run conf (Syslog.Facility.Kern, Syslog.Severity.Err);
    val it = [File "/var/adm/kernel"] : SyslogConf.action list

    - SyslogConf.run conf (Syslog.Facility.Kern, Syslog.Severity.Crit);
    val it = [File "/var/adm/kernel",File "/dev/console"] : SyslogConf.action list

    - SyslogConf.run conf (Syslog.Facility.User, Syslog.Severity.Emerg);
    val it = [] : SyslogConf.action list

# Confusing cases

    - val conf = SyslogConf.load inputLine (lines "mail.crit;*.err /var/log/messages");
    ...

    - SyslogConf.run conf (Syslog.Facility.Mail, Syslog.Severity.Crit);
    val it = [File "/var/log/messages"] : SyslogConf.action list

    - SyslogConf.run conf (Syslog.Facility.Mail, Syslog.Severity.Err);
    val it = [File "/var/log/messages"] : SyslogConf.action list

