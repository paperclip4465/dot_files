import subprocess

def mailpasswd(acct):
    args = ["pass", "show", acct]
    try:
        ret = subprocess.check_output(args).splitlines()[0]
        return ret
    except subprocess.CalledProcessError:
        return ""
