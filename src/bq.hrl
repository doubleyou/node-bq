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
    armor,
    weapon,
    modstate,
    area
}).

-record(player, {
    name,
    client_pid
}).

-record(property, {
    type,
    hp,
    armor,
    weapon,
    drops = []
}).

-record(door, {
    x,
    y,
    p,
    tcx,
    tcy,
    to,
    tx,
    ty
}).


-record(checkpoint, {
    id,
    x,
    y,
    w,
    h,
    s
}).

-record(roaming_area, {
    id,
    x,
    y,
    width,
    height,
    type,
    nb
}).

-record(chest_area, {
    x,
    y,
    w,
    h,
    i,
    tx,
    ty
}).

-record(chest, {
    x,
    y,
    i
}).

-define(json2record(Record, JSON), list_to_tuple([Record|[proplists:get_value(atom_to_binary(K,latin1),JSON) || K <- record_info(fields, Record)]])).
