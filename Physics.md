# Introduction #

A high-level view of how physics/collision will work.

# Details #

Each object will have several bounding spheres associated with it.  Hopefully the object will have a skeleton, and the spheres will be tied to parts of the skeleton. So when the skeleton moves (along with the model), the spheres will move along with it.
  * Each model should have a config file with it that specifies where the spheres are bound to.

So when checking if two sphere-bound objects collide, can simply compare each individual sphere of one object to each sphere of the other object. This can be done by comparing distance between centers to sum of radii.

Platforms, if rectangular prisms, will need to have each face checked or the closest face.  Comparing to sphere-bound objects, this requires plane-sphere intersection calculations.  Unsure if platforms will rotate.
Rectangular-rectangular interactions may be necessary for colliding platforms.

Line-sphere intersections for shooting things and line-plane intersections for shooting platforms also necessary.