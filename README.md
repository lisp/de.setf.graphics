

DE.SETF.GRAPHICS: a Common Lisp graphics library
-------

`de.setf.graphics` implements a native Common Lisp graphics kibrary.
It provides 3d scene and geometry modeling in CLOS with
rendering support for clim, clx, opengl, quickdraw, and svg.



Status
------

This library has been used for research, development, and prototyping. 
Over the years, it has been built and [probed](./tests/tests.asd) in the following combinations

<table>
<tr><td style='text-align: right;'>Graphics Interface<br/>lisp implementation</td><th>Core Graphics</th><th>xlib</th><th>Open GL</th><th>SVG</th></tr>
<tr><th>MCL</th>
    <td> </td>
    <td>MCL-5.2, clx_0.4:<br/><a href='./readmes/mcl-clx.png'><img src='http://github.com/lisp/de.setf.graphics/raw/master/readmes/mcl-clx.png' width='64' height='64'/></a></td>
    <td>MCL-5.2, AGL-1.2.4:<br/><a href='./readmes/mcl-opengl.png'><img src='http://github.com/lisp/de.setf.graphics/raw/master/readmes/mcl-opengl.png' width='64' height='64'/></a></td>
    <td>MCL-5.2, safari 4.03:<br/>
        <a href='./readmes/test-document.svg'><img type='image/svg+xml' src='http://github.com/lisp/de.setf.graphics/raw/master/readmes/test-document.svg' width='64' height='64'/></a></td></tr>
<tr><th>CCL</th>
    <td/>
    <td>CCL-1.3, OS X 4.6, clx_0.7.3:<br/><a href='./readmes/ccl-clx.png'><img src='http://github.com/lisp/de.setf.graphics/raw/master/readmes/ccl-clx.png' width='64' height='64'/></a></td>
    <td> </td>
    <td> </td></tr>
<tr><th>SBCL</th>
    <td/>
    <td>SBCL-1-0-35, OS X 4.6, clx_0.7.4:<br/><a href='./readmes/sbcl-clx.png'><img src='http://github.com/lisp/de.setf.graphics/raw/master/readmes/sbcl-clx.png' width='64' height='64'/></a></td>
    <td></td>
    <td> </td></tr>
</table>

The documentation is, unfortunately, to be found in the form of files intended for Word for Macintosh, Version 4.0, only.
Until that improves, consult the [tests](./tests/) for examples.

common-lisp.net hosts a mailing [list](http://www.common-lisp.net/mailman/listinfo/de-setf-graphics-devel)
and a [trac](http://trac.common-lisp.net/de-setf-graphics/) instance for discussions and/or reports.

Notes
-----

- angles, drawing arcs and rotating coordinate systems
  - [clim](clim rotation +radians) transform rotations and [arc angles](http://www.mikemac.com/mikemac/clim/regions.html#3.2.5) are radians.
  - common graphics [arc angles](http://www.franz.com/support/documentation/8.1/doc/operators/cg/d/draw-ellipse-arc.htm) are degrees, 
  - [core graphics](http://developer.apple.com/mac/library/documentation/GraphicsImaging/Reference/CGAffineTransform/Reference/reference.html#//apple_ref/c/func/CGAffineTransformRotate)
    rotation is in radians. Arc perations are in radians [CGContextAddArc](http://developer.apple.com/mac/library/documentation/GraphicsImaging/Reference/CGContext/Reference/reference.html#//apple_ref/doc/uid/TP30000950-CH1g-F17001)
  - [CLX](http://www.x.org/wiki/) arc angles are degrees, view rotation is computed in radians. [@xfree](http://www.xfree86.org/current/XArc.3.html)
  - [Java 3D] [rotations](http://java.sun.com/javase/technologies/desktop/java3d/forDevelopers/J3D_1_3_API/j3dapi/index.html) are radians
  - [OpenGL](http://www.opengl.org/sdk/docs/man/xhtml/glRotate.xml) degrees
  - SVG : [transforms](http://www.w3.org/TR/SVG/coords.htm) are degrees, 

No one of them is preeminent. The internal transformation operators are implemented in radians and converted as required as arguments to
library functions. It would be possible to apply internalization/externailzation operators intrface arguments, but since the interface
exposes internal levels, it's not clear where this normalization should happen. For the meantime, use radians.


Downloading
-----------

Images are in the download area for
[ccl](http://github.com/downloads/lisp/de.setf.graphics/ccl-graphics.image.tgz) and
[sbcl](http://github.com/downloads/lisp/de.setf.graphics/sbcl-graphics.core.tgz) runtimes.

Building
---------

In principle, `de.setf.graphics` is built with [`asdf`](http://www.common-lisp.net/projects/asdf).
Once one has the sources and the `asdf` configuration in place, evaluate

    (asdf:operate 'asdf:load-op :de.setf.graphics.clx)
    (asdf:operate 'asdf:load-op :de.setf.graphics.opengl)

to load the respective version. The versions intend to be compatible as each implements a distinct
graphics context for the respective library.
Please consult the detailed instructions for the respective [runtime](./readmes/README-build.md) for more information.

 
Licensing
---------

This version is released under version 3 of the GNU Affero license (GAL).[[5]]
The required components are included as per the respective licenses and covered,
in this combined form,  under the GAL as well

- agl : unknown
  - 2005 [Alexander Repenning](mailto:ralex@cs.colorado.edu)
- closer-mop :  MIT-style
  - 2005 - 2010 [Pascal Costanza](http://p-cos.net)
- clx : TI; additional attributions undated (consult the (external.lisp)[./clx/ecternal.lisp] for the reference to the
  exact version
  - 1987 : Texas Instruments
  - 1988, 1989 : Franz Inc
- meta : anon
  - 2002 [Jochen Schmidt](jsc@dataheaven.de)
- skippy : public domain
  - 2005 [Zach Beane](xach@xach.com)

This library has evolved over the years under various copyrights

- Copyright 1988-1993 [james anderson](mailto:jaa@dtmg.mit.edu) (lgpl)
  - 19880616: the original version was written as part of a research program in
 design reasoning with the "design tools and methods group" in the
 department of architecture and planning, massachusetts institute of
 technology. the basis was mcl/object-lisp/quickdraw
  - 19891107: ported to clx/pcl
- Copyright 1993-2003 [james anderson](mailto:james.anderson@setf.de) (lgpl)
  - 19940528: changed element class to use standard-object classes rather
   than structure-class, since structures fail to initialize correctly
   with an extended subclass hierarchy.
  - 20030829: ported to mcl 5.0.
   introduced abstract graphics operations and opengl support, which
   involved optional transform processing. simplified location implementation
   to eliminate typed locations.
- Copyright 2003-2004 [james anderson](mailto:janderson@ravenpack.com) (lgpl)
  - ported to allegro.
- Copyright 2004-2009 [james anderson](mailto:james.anderson@setf.de) (lgpl)
  - ported to clozure.
- Copyright 2010 [james anderson](mailto:james.anderson@setf.de) (gal)


 [5]: agpl.txt

--------
![made with mcl](http://www.digitool.com/img/mcl-made-1.gif "Made With MCL")


