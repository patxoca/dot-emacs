import os
import re
import textwrap
import logging


_re_labels = []

class File :
    """un arxiu que porta control de quin es el nro. de l'ultima linea
    llegida, tambe permet 'tornar' linees a l'arxiu per llegirles
    despres. Compte, unread utilitza un bufer FIFO:

      a = f.readline()
      b = f.readline()
      f.unread(a)
      f.unread(b)
      f.readline() == a
    """
    def __init__(self, path, mode='r') :
        self.path = path
        self.file = open(path, mode)
        self.lineno = 0
        self._unread = []

    def unread(self, line) :
        if not line.endswith('\n') :
            line = line + '\n'
        self._unread.append(line)
        self.lineno -= 1

    def readline(self) :
        if self._unread :
            l = self._unread.pop(0)
        else :
            l = self.file.readline()
        if l :
            self.lineno += 1
        return l

    def close(self) :
        self.file.close()
        self.file = None


def parse_block(label, firstline, file) :
    res = []
    index = firstline.index(label)
    left_side = firstline[:index]
    res.append(firstline[index:])
    l = file.readline()
    while l :
        l = l.rstrip('\n')
        if ((not l.startswith(left_side)) \
            or (not l[index:].rstrip(' '))) :
            file.unread(l)
            break
        res.append(l[index:])
        l = file.readline()
    return '\n'.join(res)

def parse_file(file) :
    """Analitza un arxiu i retorna una llista de llistes (nro_linea,
    etiqueta, text)
    """
    res = []
    l = file.readline()
    while l :
        l = l.rstrip('\n')
        for r in _re_labels :
             m = r.search(l)
             if m :
                 lineno = file.lineno
                 label = m.group(1)
                 text = parse_block(label, l, file)
                 res.append((lineno,
                             label.upper(),
                             text)
                            #textwrap.fill(textwrap.dedent(text),
                            #              replace_whitespace=True))
                            )
        l = file.readline()
    return res

def file_walker(top,
                filtre_arxiu=lambda x: True,
                filtre_dir=lambda x: True) :
    """Visita tots els arxius en el directori top i
    subdirectoris. filtre_arxiu i filtre_dir son dos funcions que son
    cridades per cada directori i arxiu, si retornen True els
    processar l'arxiu/directori, si retornen False se l'ignora. La
    signatura d'aquestes funcions es:

      filtre_arxiu(directori, nom_arxiu)

      filtre_dir(directori, nom_directori)

    Retorna els todos i fixmes en algun format encara no especificat
    ...

      ((arxiu, nro_linea, etiqueta, text), ...)

    arxiu inclou el directori. COMPTE!! una tupla es veu al costat
    emacs com un vector, no com una llista
    """
    res = []
    if top.endswith('/') :
        top = top[:-1]
    for (dirpath, dirnames, filenames) in os.walk(top) :
        for filename in filenames :
            if filtre_arxiu(dirpath, filename) :
                #logging.debug(dirpath + "/" + filename)
                file = File(dirpath + '/' + filename, 'r')
                for match in parse_file(file) :
                    res.append((dirpath + '/' + filename, ) + match)
                file.close()

        to_delete = [ dir for dir in dirnames if not filtre_dir(dirpath, dir) ]
        for dir in to_delete :
            dirnames.remove(dir)
    return res

def init_labels(labels) :
    global _re_labels
    _re_labels = []
    for l in labels :
        _re_labels.append(re.compile('(' + l + ')', re.IGNORECASE))
