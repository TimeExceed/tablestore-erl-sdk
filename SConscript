# -*- python -*-
Import('env')

env.subDir('src')
# env.subDir('external')
env.subDir('test')

env.tarball('aliyun-tablestore-sdk-erlang-4.0.0.tar.gz',
            [('include/', 'src/ots.hrl'),
             ('lib/', env.Glob('src/*.beam'))])

