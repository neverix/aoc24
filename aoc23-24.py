import numpy as np
t = list(map(lambda x: x.encode("ascii"), open("input.txt").read().split("\n")))
x = np.stack(list(map(lambda x: np.frombuffer(x, dtype=np.uint8), t)), 0)
h, w = x.shape
i = np.arange(x.size).reshape(x.shape)
for _ in range(100):
    m = ((x[:, :-1] == x[:, 1:]) & (i[:, :-1] > i[:, 1:]))
    i[:, :-1][m] = i[:, 1:][m]
    m = ((x[:, 1:] == x[:, :-1]) & (i[:, 1:] > i[:, :-1]))
    i[:, 1:][m] = i[:, :-1][m]
    m = ((x[:-1] == x[1:]) & (i[:-1] > i[1:]))
    i[:-1][m] = i[1:][m]
    m = ((x[1:] == x[:-1]) & (i[1:] > i[:-1]))
    i[1:][m] = i[:-1][m]
tot = 0
for reg in np.unique(i.ravel()):
    u = (i == reg)
    v = np.copy(u)
    u = np.concatenate((u[:2] * 0, u, u[:2] * 0), 0)
    # u = np.concatenate((u[:, :1] * 0, u, u[:, :1] * 0), axis=1)
    u = vv = np.concatenate((u[:, :2] * 0, u, u[:, :2] * 0), axis=1)
    u = np.stack((u, u, u, u), axis=-1).reshape(*u.shape, 2, 2).transpose(0, 2, 1, 3).reshape(u.shape[0] * 2, -1)
    # print(u)
    peri = 0
    def redum(x):
        x = x.astype(np.int32)
        x = x.T
        for z in range(x.shape[0] - 1):
            x[z+1] = x[z+1] * (x[z] + 1)
        return x.T
    peri += (redum(u[1:] != u[:-1]) == 1).sum()
    print(peri)
    u = u.T
    peri += (redum(u[1:] != u[:-1]) == 1).sum()
    print(peri)
    
    # vv = u
    diag = np.stack((vv[:-1, :-1], vv[:-1, 1:], vv[1:, :-1], vv[1:, 1:]), -1)
    pp = (diag == np.array([1, 0, 0, 1], dtype=np.bool_)).all(axis=-1) | (diag == np.array([0, 1, 1, 0], dtype=np.bool_)).all(axis=-1)
    peri += pp.sum() * 2
    price = v.sum() * peri
    print(x[v].max(), v.sum(), peri, price, pp.sum())
    tot += price
print(tot)