-record(client, {
    upstream,
    logged_in = false,
    armor,
    weapon,
    id,
    x,
    y,
    orientation = random:uniform(4),
    checkpoint,
    hitpoints,
    target
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
    hitpoints,
    opts = []
}).


-record(property, {
    type,
    hp,
    armor,
    weapon,
    drops = []
}).

-define(json2record(Record, JSON), list_to_tuple([Record|[proplists:get_value(atom_to_binary(K,latin1),JSON) || K <- record_info(fields, Record)]])).

