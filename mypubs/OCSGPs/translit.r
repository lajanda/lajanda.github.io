translitgr <- function(x) {
a = gsub('ἄχρι','akhri',x)
b = gsub('ἀνά','ana', a)
c = gsub('ἄνευ','aneu', b)
d = gsub('ἀντί','anti', c)
e = gsub('ἄντικρυς','antikrus', d)
f = gsub('ἀντιπέρα','antipera', e)
g = gsub('ἀπέναντι','apenanti', f)
h = gsub('ἄτερ','aper', g)
i = gsub('ἀπό','apo', h)
j = gsub('διά','dia', i)
k = gsub('ἐγγύς','engus', j)
l = gsub('εἰς','eis', k)
m = gsub('ἐκτός','ektos', l)
n = gsub('ἐκ','ek', m)
o = gsub('ἔμπροσθεν','emprosthen', n)
p = gsub('ἔναντι','enanti', o)
q = gsub('ἐναντίον','enantion', p)
r = gsub('ἕνεκα','heneka', q)
s = gsub('ἐνώπιον','enopion', r)
t = gsub('ἐντός','entos', s)
u = gsub('ἐν','en', t)
v = gsub('ἐπάνω','epano', u)
w = gsub('ἐπί','epi', v)
y = gsub('ἔξωθεν','exothen', w)
z = gsub('ἔξω','exo', y)
aa = gsub('ἅμα','hama', z)
bb = gsub('ἕως','heos', aa)
cc = gsub('ὑποκάτω','hupokato', bb)
dd = gsub('ὑπέρ','huper', cc)
ee = gsub('ὑπεράνω','huperano', dd)
ff = gsub('ὑπερέκεινα','hyperekeina', ee)
gg = gsub('ὑπό','hypo', ff)
hh = gsub('κατά','kata', gg)
ii = gsub('κατέναντι','katenanti', hh)
jj = gsub('κατενώπιον','katenopion', ii)
kk = gsub('χάριν','kharin', jj)
ll = gsub('χωρίς','khoris', kk)
mm = gsub('κυκλόθεν','kyklothen', ll)
nn = gsub('μέχρι','mekhri', mm)
oo = gsub('μετά','meta', nn)
pp = gsub('μεταξύ','metaxu', oo)
qq = gsub('ὀπίσω','opiso', pp)
rr = gsub('ὄπισθεν','opisthen', qq)
ss = gsub('παρά','para', rr)
tt = gsub('παρεκτὸς','parektos', ss)
uu = gsub('πέραν','peran', tt)
vv = gsub('περί','peri', uu)
ww = gsub('πλήν','plen', vv)
xx = gsub('πλησίον','plesion', ww)
yy = gsub('πρός','pros', xx) 
zz = gsub('πρό','pro', yy)
sist = gsub('σύν','syn', zz)
sist

}

translitocs <- function(x) {
a = gsub('а', 'a', x)
b = gsub('б', 'b', a)
c = gsub('в', 'v', b)
d = gsub('г', 'g', c)
e = gsub('д', 'd', d)
f = gsub('е', 'e', e)
g = gsub('ж', 'ž', f)
h = gsub('з', 'z', g)
i = gsub('и', 'i', h)
j = gsub('к', 'k', i)
k = gsub('л', 'l', j)
l = gsub('м', 'm', k)
m = gsub('н', 'n', l)
n = gsub('оу', 'u', m)
o = gsub('п', 'p', n)
p = gsub('р', 'r', o)
q = gsub('с', 's', p)
r = gsub('т', 't', q)
s = gsub('о', 'o', r)
#t = gsub('ъ', '', s)
u = gsub('ѣ', 'ě', s)
v = gsub('ю', 'ju', u)
#w = gsub('ь', '', v)
x = gsub('ѫ', 'o', v)
y = gsub('ѥ', 'je', x)
z = gsub('ꙗ', 'ja', y)
aa = gsub('ѵ', 'y', z)
bb = gsub('ѧ', 'ę', aa)
cc = gsub('ѭ', 'jǫ', bb)
dd = gsub('і', 'i', cc)
ee = gsub('ш', 'š', dd)
ff = gsub('ц', 'c', ee)
gg = gsub('ꙑ', 'y', ff)
hh = gsub('ѩ', 'ję', gg)
ii = gsub('ꙇ', 'i', hh)
jj = gsub('ѕ', 'z', ii)
kk = gsub('щ', 'št', jj)
ll = gsub('ч', 'č', kk)
mm = gsub('х', 'x', ll)
nn = gsub('ф', 'f', mm)
nn
}

translitgot <- function(x) {
a = gsub('þ','th', x)
b = gsub('ƕ','hw', a)
b
}















