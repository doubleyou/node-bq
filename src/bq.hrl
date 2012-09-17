-record(actor, {
    id,
    type,
    module,
    x = 16,
    y = 210,
    orientation = random:uniform(4),
    hp = 100,
    max_hp = 100,
    regen_rate = 1,
    haters = [],
    pid,
    modstate
}).

-record(player, {
    id,
    name,
    client_pid,
    armor,
    weapon
}).
