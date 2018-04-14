# Setup

    - CM.make "syslogd.cm";
    ...
    val it = true : bool

    - val inputLine = TextIO.StreamIO.inputLine;
    ...
    - val lines = TextIO.getInstream o TextIO.openString;
    ...

# All crit or severer messages, except for kern

    - val conf = Syslog.Conf.load inputLine (lines "*.crit;kern.none /var/adm/critical");
    ...

    - Syslog.Conf.run conf (Syslog.User, Syslog.Crit);
    val it = [File "/var/adm/critical"] : Syslog.Conf.action list

    - Syslog.Conf.run conf (Syslog.User, Syslog.Alert);
    val it = [File "/var/adm/critical"] : Syslog.Conf.action list

    - Syslog.Conf.run conf (Syslog.User, Syslog.Err);
    val it = [] : Syslog.Conf.action list

    - Syslog.Conf.run conf (Syslog.Kern, Syslog.Crit);
    val it = [] : Syslog.Conf.action list

    - Syslog.Conf.run conf (Syslog.Kern, Syslog.Alert);
    val it = [] : Syslog.Conf.action list

    - Syslog.Conf.run conf (Syslog.Kern, Syslog.Err);
    val it = [] : Syslog.Conf.action list

# All kern messages

    - val conf = Syslog.Conf.load inputLine (lines "kern.* /var/adm/kern");
    ...

    - Syslog.Conf.run conf (Syslog.Kern, Syslog.Emerg);
    val it = [File "/var/adm/kern"] : Syslog.Conf.action list

    - Syslog.Conf.run conf (Syslog.Kern, Syslog.Alert);
    val it = [File "/var/adm/kern"] : Syslog.Conf.action list

    - Syslog.Conf.run conf (Syslog.Kern, Syslog.Crit);
    val it = [File "/var/adm/kern"] : Syslog.Conf.action list

    - Syslog.Conf.run conf (Syslog.Kern, Syslog.Err);
    val it = [File "/var/adm/kern"] : Syslog.Conf.action list

    - Syslog.Conf.run conf (Syslog.Kern, Syslog.Warning);
    val it = [File "/var/adm/kern"] : Syslog.Conf.action list

    - Syslog.Conf.run conf (Syslog.Kern, Syslog.Notice);
    val it = [File "/var/adm/kern"] : Syslog.Conf.action list

    - Syslog.Conf.run conf (Syslog.Kern, Syslog.Info);
    val it = [File "/var/adm/kern"] : Syslog.Conf.action list

    - Syslog.Conf.run conf (Syslog.Kern, Syslog.Debug);
    val it = [File "/var/adm/kern"] : Syslog.Conf.action list

    - Syslog.Conf.run conf (Syslog.User, Syslog.Emerg);
    val it = [] : Syslog.Conf.action list

# mail.info and news.info

    - val conf = Syslog.Conf.load inputLine (lines "mail,news.info /var/adm/info");
    ...

    - Syslog.Conf.run conf (Syslog.Mail, Syslog.Info);
    val it = [File "/var/adm/info"] : Syslog.Conf.action list

    - Syslog.Conf.run conf (Syslog.New, Syslog.Info);
    val it = [File "/var/adm/info"] : Syslog.Conf.action list

    - Syslog.Conf.run conf (Syslog.User, Syslog.Info);
    val it = [] : Syslog.Conf.action list

# Multiple rules

    - val conf = Syslog.Conf.load inputLine (lines "kern.* /var/adm/kernel\nkern.crit /dev/console\n");
    ...

    - Syslog.Conf.run conf (Syslog.Kern, Syslog.Err);
    val it = [File "/var/adm/kernel"] : Syslog.Conf.action list

    - Syslog.Conf.run conf (Syslog.Kern, Syslog.Crit);
    val it = [File "/var/adm/kernel",File "/dev/console"]
      : Syslog.Conf.action list

    - Syslog.Conf.run conf (Syslog.User, Syslog.Emerg);
    val it = [] : Syslog.Conf.action list

# Confusing cases

    - val conf = Syslog.Conf.load inputLine (lines "mail.crit;*.err /var/log/messages");
    ...

    - Syslog.Conf.run conf (Syslog.Mail, Syslog.Crit);
    val it = [File "/var/log/messages"] : Syslog.Conf.action list

    - Syslog.Conf.run conf (Syslog.Mail, Syslog.Err);
    val it = [File "/var/log/messages"] : Syslog.Conf.action list

