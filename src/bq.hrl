-record(client, {
    upstream,
    logged_in = false,
    armor = 21,
    weapon = 60,
    id = 1000,
    x = 0,
    y = 0,
    orientation = random:uniform(4),
    checkpoint
}).



-record(world, {
    width,
    height,
    collisions = [],
    doors = [],
    checkpoints = [],
    roaming_areas = [],
    chest_areas = [],
    static_chests = [],
    static_entities = [],
    titlesize,
    entities = [],
    buildings = [],
    characters = []
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


-record(entity, {
    id,
    type,
    x,
    y,
    orient,
    name,
    armor,
    weapon,
    pid,
    opts = []
}).

-define(json2record(Record, JSON), list_to_tuple([Record|[proplists:get_value(atom_to_binary(K,latin1),JSON) || K <- record_info(fields, Record)]])).

