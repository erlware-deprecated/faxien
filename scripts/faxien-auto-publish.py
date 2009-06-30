#!/usr/bin/env python

"""
This Python script will help you download and compile an erlang
and publish erts and the built-in applications with faxien.
"""

import optparse, os, urllib, re, sets, subprocess, sys, tempfile, time

VERSION = '0.1.1'

DOWNLOAD_URL = 'http://erlang.org/download'

INTERACTIVE = os.isatty(sys.stdin.fileno())

WORKING_DIR = os.path.join(tempfile.gettempdir(), 'faxien-auto-publish')

IS_64_BIT = sys.maxint > 2**31 - 1

# re to find the source versions available for download
scraper = re.compile(r'otp_src_(R)(\d+)(\w+)(?:-(\d+))?\.tar.gz')

# re to split erlang version numbers
erlang_verpat = re.compile(r'(R)(\d+)(\w+)-(\d+)')

# ioctl_GWINSZ and terminal_size are from Chuck Blake's cls.py
# http://pdos.csail.mit.edu/~cblake/cls/cls.py

def ioctl_GWINSZ(fd):                  #### TABULATION FUNCTIONS
    try:                                ### Discover terminal width
        import fcntl, termios, struct
        cr = struct.unpack('hh', fcntl.ioctl(fd, termios.TIOCGWINSZ, '1234'))
    except:
        return None
    return cr

def terminal_size():                    ### decide on *some* terminal size
    cr = ioctl_GWINSZ(0) or ioctl_GWINSZ(1) or ioctl_GWINSZ(2)  # try open fds
    if not cr:                                                  # ...then ctty
        try:
            fd = os.open(os.ctermid(), os.O_RDONLY)
            cr = ioctl_GWINSZ(fd)
            os.close(fd)
        except:
            pass
    if not cr:                            # env vars or finally defaults
        try:
            cr = (os.environ['LINES'], os.environ['COLUMNS'])
        except:
            cr = (25, 80)
    return int(cr[1]), int(cr[0])         # reverse rows, cols


def check_working_dir(working_dir):
    """Check to make sure we can use the given working directory."""

    first_existing = working_dir

    while True:
        if os.path.exists(first_existing):
            break

        up_one = os.path.dirname(first_existing)
        if up_one == first_existing:
            raise Exception('weird. no root path')

        first_existing = up_one

    if not os.path.isdir(first_existing):
        print >>sys.stderr, '%s is not a directory' % first_existing
        sys.exit(1)

    if not os.access(first_existing, os.R_OK | os.W_OK | os.X_OK):
        print >>sys.stderr, 'You do not have permissions to change %s.' % first_existing
        sys.exit(1)


def get_versions():
    """Return the list of versions available for download.

    Versions are returned as tuples ('R', NUMBER, STRING, NUMBER)

    So R12B-1 would be ('R', 12, 'B', 1)

    Versions are returned in descending order.
    """

    page = urllib.urlopen(DOWNLOAD_URL).read()

    versions = list(sets.Set(scraper.findall(page)))
    versions = [list(v) for v in versions]

    for v in versions:
        v[1] = int(v[1])
        v[3] = v[3] and int(v[3]) or 0

    versions = map(tuple, versions)
    versions.sort()
    versions.reverse()

    return versions


def format_version(version):
    """Format a version tuple (see get_versions) as a string."""
    if version[-1] == 0:
        return '%s%d%s' % version[:-1]
    return '%s%d%s-%d' % version


def choose_version(versions):
    """Present a menu for the user to choose a version and return
    the version chosen."""

    if not INTERACTIVE:
        print >>sys.stderr, 'choose mode in a non-interactive environment'
        sys.exit(1)

    while True:
        print
        print 'Choose a version from the list:'
        for i, version in enumerate(versions):
            print ' %d: %s' % (i + 1, format_version(version))

        ans = raw_input('[%d-%d] ' % (1, len(versions)))

        if not ans:
            print 'Aborting.'
            sys.exit(1)

        ans = ans.isdigit() and int(ans) or None

        if ans and ans >= 1 and ans <= len(versions):
            return versions[ans-1]

        print
        print 'Please enter a number between %d and %d.' % (1, len(versions))


def determine_version(options):
    """Determine which bootstrapper to use based on the options."""

    if options.erlang_version:
        match = erlang_verpat.match(options.erlang_version)
        if not match:
            print 'Expecting version like R12B-3. Got', options.erlang_version
            sys.exit(1)

        version = list(match.groups())
        version[1] = int(version[1])
        version[3] = int(version[3])
        return tuple(version)

    print 'Fetching list of erlang versions from %s' % DOWNLOAD_URL

    versions = get_versions()

    if not versions:
        print 'Did not find any versions to download at %s' % DOWNLOAD_URL
        sys.exit(1)

    if not options.choose:
        return versions[0]

    return choose_version(versions)


SPINNER = "-\|/"
TERMINAL_SIZE = None

def progress_bar(block_count, block_size, total_bytes):
    """Progress hook for urllib.urlretrieve.
    With an interactive shell, display a progress bar for download progress."""

    if not INTERACTIVE:
        return

    if block_count % 10 != 0:
        return

    count = block_count // 10

    progress_length = max(0, TERMINAL_SIZE[0] - 6)

    bytes = block_count * block_size

    num_bars = progress_length * (bytes / float(total_bytes))
    num_bars = int(num_bars)

    if num_bars:
        last_bars = progress_length * ((bytes - (block_size * 10)) / float(total_bytes))
        last_bars = int(last_bars)
        new_bars = '=' * (num_bars - last_bars)
    else:
        new_bars = ''

    percent = 100 * (bytes / float(total_bytes))
    percent = ' %3d%%' % round(percent)

    sys.stdout.write('\b\b\b\b\b\b' + new_bars + SPINNER[count % len(SPINNER)] + percent)
    sys.stdout.flush()


def already_downloaded(tarfile):
    """Return True if the given tarfile has already been downloaded."""

    if not os.path.exists(tarfile):
        return False

    url = '%s/%s' % (DOWNLOAD_URL, os.path.basename(tarfile))

    content_length = urllib.urlopen(url).info().getheader('Content-Length')
    if not content_length or not content_length.isdigit():
        return False

    content_length = int(content_length)

    current_size = os.stat(tarfile).st_size

    return content_length == current_size


def download_tarfile(tarfile):
    """Fetch the given tarfile from the website. Return the file location."""

    global TERMINAL_SIZE
    if INTERACTIVE:
        TERMINAL_SIZE = terminal_size()

    url = '%s/%s' % (DOWNLOAD_URL, os.path.basename(tarfile))

    print 'Downloading: %s' % url
    print 'Destination: %s' % tarfile

    if INTERACTIVE:
        sys.stdout.write(SPINNER[0] + '    %')

    urllib.urlretrieve(url, tarfile, reporthook=progress_bar)[0]

    print
    print 'Download done.'


def spin_wait(process):
    """Wait for a process to finish while animating a spinner."""

    count = 0

    if INTERACTIVE:
        sys.stdout.write(' ')

    while True:
        if INTERACTIVE:
            sys.stdout.write('\b%s' % SPINNER[count % len(SPINNER)])
            sys.stdout.flush()

        if process.poll() is not None:
            break

        time.sleep(.2)

        count += 1

    if INTERACTIVE:
        sys.stdout.write('\b')

    return process.returncode


def process_failed(returncode, stdout):
    """Return True if the process failed."""

    if returncode != 0:
        return True
    if 'badly_formatted' in stdout:
        return True
    return False


def run_command(cmd, run_dir, log_dir, name, critical=True):
    """Run the command with output to logs."""

    sys.stdout.write('[%s] ' % name)
    sys.stdout.flush()

    flag_file = os.path.join(log_dir, '%s.done' % name)
    if os.path.exists(flag_file):
        print 'already done'
        return

    stdin = open('/dev/null')
    stdout = open(os.path.join(log_dir, '%s.stdout' % name), 'w')
    stderr = open(os.path.join(log_dir, '%s.stderr' % name), 'w')

    stdout.write('Executing %s in %s\n' % (cmd, run_dir))
    stdout.flush()

    process = subprocess.Popen(cmd, cwd=run_dir,
                               stdin=stdin, stdout=stdout, stderr=stderr)

    returncode = spin_wait(process)

    if process_failed(returncode, open(stdout.name).read()):
        print 'failed'
        if critical:
            sys.exit(1)
    else:
        print 'done'
        open(flag_file, 'w').close()


def unpack_tarfile(tarfile, log_dir):
    """Untar the erlang tarball into the working directory."""

    cmd = ['tar', 'xzf', tarfile]

    run_command(cmd, os.path.dirname(tarfile), log_dir, 'unpack')

    src_dir = tarfile[:-len('.tar.gz')]

    if not os.path.isdir(src_dir):
        print 'Did not find source directory:', src_dir
        sys.exit(1)

    return src_dir


def configure_src(src_dir, install_dir, log_dir):
    """Configure the source directory. Return the install directory."""

    cmd = ['./configure', '--enable-kernel-poll', '--enable-smp-support',
           '--disable-sctp', '--prefix', install_dir]

    run_command(cmd, src_dir, log_dir, 'configure')


def make(src_dir, log_dir):
    """Build the source."""

    run_command(['make'], src_dir, log_dir, 'make')


def install(src_dir, install_dir, log_dir):
    """Install the source."""

    run_command(['make', 'install'], src_dir, log_dir, 'install')

    if not os.path.isdir(install_dir):
        print 'Installation not made:', install_dir
        sys.exit(1)


def make_app_files(install_dir, log_dir):
    """Use the genapp.erl script to give .app files to built-in
    libs that do no have them."""

    escript = os.path.join(install_dir, 'bin', 'escript')
    lib_dir = os.path.join(install_dir, 'lib', 'erlang', 'lib')
    run_dir = os.path.abspath(os.path.dirname(sys.argv[0]))
    genapp = os.path.join(run_dir, 'genapp.erl')

    if not os.path.exists(genapp):
        print 'Missing genapp.erl:', genapp
        sys.exit(1)

    run_command([escript, genapp, lib_dir], run_dir, log_dir, 'genapp')


def get_erts_dir(erlang_dir):
    """Return the erts sub-directory in the erlang installation directory."""

    erts_dirs = [f for f in os.listdir(erlang_dir) if f.startswith('erts-')]

    if not erts_dirs:
        print 'No erts directory found in :', erlang_dir
        sys.exit(1)

    if len(erts_dirs) > 1:
        print 'Multiple erts directories found in :', erlang_dir
        sys.exit(1)

    return os.path.join(erlang_dir, erts_dirs[0])


def get_target_erts_version():
    """Return the current target erts version of faxien."""

    cmd = ['faxien', 'show-target-erts-vsn']

    p = subprocess.Popen(cmd, stdout=subprocess.PIPE)

    (out, _) = p.communicate()

    if p.returncode != 0:
        print 'Faxien call failed:', cmd
        sys.exit(1)

    return out.strip()


def publish(dir, log_dir):
    """Publish the given directory with faxien."""

    name = 'publish-%s' % os.path.basename(dir).split('-')[0]

    run_command(['faxien', 'publish', dir, 'infinity'],
                log_dir, log_dir, name, False)


def get_application_dirs(erlang_dir):
    """Return a list of application directories."""

    app_dirs = []

    lib_dir = os.path.join(erlang_dir, 'lib')

    for basename in os.listdir(lib_dir):
        if '-' not in basename:
            continue

        app_dir = os.path.join(lib_dir, basename)

        if not os.path.isdir(app_dir):
            continue

        app_dirs.append(app_dir)

    app_dirs.sort()

    return app_dirs


def get_published_apps(version):
    """Return the current published apps for the given erlang version."""

    cmd = ['faxien', 'search', format_version(version), 'both', 'normal', '""']

    p = subprocess.Popen(cmd, stdout=subprocess.PIPE)

    (out, _) = p.communicate()

    if p.returncode != 0:
        print 'Faxien call failed:', cmd
        sys.exit(1)

    apps = []

    lines = out.strip().split('\n')

    while lines and not lines[0].startswith('Applications'):
        lines.pop(0)

    if lines:
        lines.pop(0)

    while lines and lines[0].startswith(' '):
        apps.append(lines.pop(0).strip())

    return apps


BINARY_FILE_EXTENSIONS = ["cmx", "py", "bat", "exe", "so"]
BINARY_FILE_REGEX = ["ELF .* executable", 
                     "shared object", 
                     "dynamically linked",
                     "ar archive"]

def get_file_type(filename):
    from subprocess import Popen, PIPE
    return Popen(["file", "-b", filename], stdout=PIPE).communicate()[0].strip()

def is_binary_lib(app_dir):
    """Is the application binary? (i.e., has hardware specific code).

    This function should be kept in sync with is_package_a_binary_app/1
    in the epkg_validation module.
    """

    for name in os.listdir(app_dir):
        if name.endswith('_src'):
            return True

    for root, _, files in os.walk(app_dir):
        for name in files:
            if os.path.splitext(name)[1][1:] in BINARY_FILE_EXTENSIONS:
                return True

            # Check the "file -b" command against each file name and see if it returns
            # a value that matches one of a list of regexs
            file_type = get_file_type(os.path.join(root, name))
            for r in BINARY_FILE_REGEX:
                if re.search(r, file_type):
                    return True

    # Finally, check .ebin file and see if there is a {force_binary_app, true} present
    (app, vsn) = os.path.basename(app_dir).split("-")
    app_file = os.path.join(app_dir, "ebin", "%s.app" % (app))
    if os.path.exists(app_file):
        app_data = str(open(app_file, "r").readlines())
        if re.match(".*{\s*force_binary_app\s*,\s*true}.*", app_data):
            return True

    return False

def remove_shell_scripts(erts_dir):
    """ Remove any shell scripts found within the erts_dir; we provide our own within erlware """
    bin_dir = os.path.join(erts_dir, "bin")
    for root, _, files in os.walk(bin_dir):
        for name in files:
            filename = os.path.join(root, name)
            file_type = get_file_type(filename)
            if file_type.find("shell script text") != -1:
                os.remove(filename)

def clone_bootstrap(working_dir, log_dir):
    """Clone the bootstrap repo into the working directory."""

    run_command(['git', 'clone', 'git://github.com/erlware/bootstrap.git'],
                working_dir, log_dir, 'clone-bootstrap')

    bootstrap_dir = os.path.join(working_dir, 'bootstrap')

    if not os.path.exists(bootstrap_dir):
        print 'Missing bootstrap directory:', bootstrap_dir
        sys.exit(1)

    return bootstrap_dir


def create_bootstrap(bootstrap_dir, erts_dir, log_dir):
    """Create the bootstrapper using the create_bootstrap.sh script."""

    run_command(['./create_bootstrap.sh', erts_dir], bootstrap_dir,
                log_dir, 'create-bootstrap')

    files = os.listdir(bootstrap_dir)
    for name in files:
        if name.startswith('faxien-launcher-') and name.endswith('.sh'):
            return os.path.join(bootstrap_dir, name)

    print 'No boostrap file in:', bootstrap_dir


def make_bootstrapper(working_dir, erts_dir, log_dir):
    """Make a bootstrapper from the erts directory."""

    bootstrap_dir = clone_bootstrap(working_dir, log_dir)
    bootstrap_file = create_bootstrap(bootstrap_dir, erts_dir, log_dir)

    print 'The bootstrap file was created here:', bootstrap_file


def main():
    usage = """%prog [options]

    Without options, this script will download the latest version of
    the erlang source, compile it, and optionally publish it with faxien.
    Additional options allow you to override the default choice or
    choose a specific version from the list of available ones.

    For more information on Erlware, see http://erlware.org

    For help, contact the erlware-questions mailing list here:

        http://groups.google.com/group/erlware-questions
    """

    parser = optparse.OptionParser(usage=usage)

    help = 'instead of publishing, create a bootstrapper'
    parser.add_option("-b", "--bootstrapper", action='store_true', help=help)

    help = 'let me choose the version to use from the current available'
    parser.add_option("-c", "--choose", action='store_true', help=help)

    help = 'do everything but actually publish'
    parser.add_option("-d", "--dry-run", action='store_true', help=help)

    help = 'specify the erlang version to use'
    parser.add_option("-e", "--erlang-version", type='str', help=help)

    help = 'print version information for this script'
    parser.add_option("-v", "--version", action='store_true', help=help)

    help = 'the working directory in which to build erlang'
    parser.add_option("-w", "--working-dir", type='str',
                      default=WORKING_DIR, help=help)

    options, args = parser.parse_args()

    if len(args) > 1:
        parser.error('bad arguments')

    if options.version:
        print 'Faxien erlang publisher version %s.' % VERSION
        sys.exit()

    working_dir = os.path.abspath(os.path.expanduser(options.working_dir))

    print 'Using working dir:', working_dir

    check_working_dir(working_dir)

    version = determine_version(options)

    print 'Using version:', format_version(version)

    working_dir = os.path.join(working_dir, format_version(version))

    if not os.path.exists(working_dir):
        os.makedirs(working_dir)

    tarfile = 'otp_src_%s.tar.gz' % format_version(version)

    local_tarfile = os.path.join(working_dir, tarfile)

    if already_downloaded(local_tarfile):
        print 'Already downloaded', local_tarfile
    else:
        download_tarfile(local_tarfile)

    log_dir = os.path.join(working_dir, 'logs')
    if not os.path.exists(log_dir):
        os.makedirs(log_dir)

    print
    print 'Starting publish steps. Log directory:', log_dir
    print

    src_dir = unpack_tarfile(local_tarfile, log_dir)

    install_dir = os.path.join(os.path.dirname(src_dir), 'install')

    configure_src(src_dir, install_dir, log_dir)
    make(src_dir, log_dir)
    install(src_dir, install_dir, log_dir)

    print
    print 'Figuring out what to publish.'
    print

    erlang_dir = os.path.join(install_dir, 'lib', 'erlang')

    if not os.path.isdir(erlang_dir):
        print 'Erlang dir missing:', erlang_dir
        sys.exit(1)

    erts_dir = get_erts_dir(erlang_dir)

    if options.bootstrapper:
        make_bootstrapper(working_dir, erts_dir, log_dir)
        sys.exit(0)

    make_app_files(install_dir, log_dir)

    # Remove shell scripts from bin/ dir in ERTS package
    remove_shell_scripts(erts_dir)

    publish_dirs = get_application_dirs(erlang_dir)

    publish_dirs.insert(0, erts_dir)

    published_apps = get_published_apps(version)

    for pub_dir in publish_dirs:
        appname = os.path.basename(pub_dir).split('-')[0]

        if appname in published_apps:
            print 'Skipping', appname, '(already published)'
            continue

        if not is_binary_lib(pub_dir) and IS_64_BIT:
            print 'Skipping', appname, '(generic and we are on 64-bit platform)'
            continue

        if options.dry_run:
            print 'Would publish:', appname
        else:
            publish(pub_dir, log_dir)



if __name__ == '__main__':
    main()
