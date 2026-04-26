; GIMP Shape Paths
; Copyright (c) 2004-2026 Jonathan Stipe and Pucelo

; ---------------------------------------------------------------------

; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.

; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.

; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define (get-kappa) (* (/ (- (sqrt 2) 1) 3) 4))

(define (translate-point x y angle distance)
  (let* ((ang (* angle (/ (* 4 (atan 1.0)) 180)))
	 (nx (+ (* distance (cos ang)) x))
	 (ny (+ (* distance (sin ang)) y)))
    (list nx ny)
  )
)

(define (make-vector-layer img pathvector fcolor scolor fillpath strokepath)
  (let* ((vectorlayer (car (gimp-vector-layer-new img pathvector))))
		(gimp-vector-layer-set-fill-color vectorlayer fcolor)
		(gimp-vector-layer-set-stroke-color vectorlayer scolor)
		(gimp-image-insert-layer img vectorlayer 0 -1)
		(gimp-vector-layer-set-enable-fill vectorlayer fillpath)
		(gimp-vector-layer-set-enable-stroke vectorlayer strokepath)
	)
)

(define (script-fu-rectangle-path img
				  drawable
				  name
				  top
				  left
				  bottom
				  right
				  strokepath
				  scolor
				  fillpath
				  fcolor)
  (gimp-image-undo-group-start img)
  (let* ((pathvector (car (gimp-path-new img name)))
	 (strokeid (car (gimp-path-bezier-stroke-new-moveto pathvector left top))))
    (gimp-path-bezier-stroke-lineto pathvector strokeid right top)
    (gimp-path-bezier-stroke-lineto pathvector strokeid right bottom)
    (gimp-path-bezier-stroke-lineto pathvector strokeid left bottom)
    (gimp-path-stroke-close pathvector strokeid)
    (gimp-image-insert-path img pathvector 0 -1)
		(make-vector-layer img pathvector fcolor scolor fillpath strokepath)
    (gimp-displays-flush)
  )
  (gimp-image-undo-group-end img))

(define (script-fu-square-path img
			       drawable
			       name
			       top
			       left
			       size
			       strokepath
			       scolor
			       fillpath
			       fcolor)
  (gimp-image-undo-group-start img)
  (let* ((pathvector (car (gimp-path-new img name)))
	 (strokeid (car (gimp-path-bezier-stroke-new-moveto pathvector left top)))
	 (right (+ left size))
	 (bottom (+ top size)))
    (gimp-path-bezier-stroke-lineto pathvector strokeid right top)
    (gimp-path-bezier-stroke-lineto pathvector strokeid right bottom)
    (gimp-path-bezier-stroke-lineto pathvector strokeid left bottom)
    (gimp-path-stroke-close pathvector strokeid)
    (gimp-image-insert-path img pathvector 0 -1)
		(make-vector-layer img pathvector fcolor scolor fillpath strokepath)
    (gimp-displays-flush)
  )
  (gimp-image-undo-group-end img))

(define (script-fu-circle-path img
			       drawable
			       name
			       x
			       y
			       radius
			       strokepath
			       scolor
			       fillpath
			       fcolor)
  (gimp-image-undo-group-start img)
  (let* ((pathvector (car (gimp-path-new img name)))
	 (top (- y radius))
	 (right (+ x radius))
	 (bottom (+ y radius))
	 (left (- x radius))
	 (clength (* radius (get-kappa)))
	 (cup (- y clength))
	 (cright (+ x clength))
	 (cdown (+ y clength))
	 (cleft (- x clength))
	 (strokeid (car (gimp-path-bezier-stroke-new-moveto pathvector x top))))
    (gimp-path-bezier-stroke-cubicto pathvector strokeid cright top right cup right y)
    (gimp-path-bezier-stroke-cubicto pathvector strokeid right cdown cright bottom x bottom)
    (gimp-path-bezier-stroke-cubicto pathvector strokeid cleft bottom left cdown left y)
    (gimp-path-bezier-stroke-cubicto pathvector strokeid left cup cleft top x top)
    (gimp-path-stroke-close pathvector strokeid)
    (gimp-image-insert-path img pathvector 0 -1)
		(make-vector-layer img pathvector fcolor scolor fillpath strokepath)
    (gimp-displays-flush)
  )
  (gimp-image-undo-group-end img))

(define (script-fu-ellipse-path img
				drawable
				name
				x
				y
				radiusx
				radiusy
				rotation
				strokepath
				scolor
				fillpath
				fcolor)
  (gimp-image-undo-group-start img)
  (let* ((pathvector (car (gimp-path-new img name)))
	 (kappa (get-kappa))
	 (clengthx (* radiusx kappa))
	 (clengthy (* radiusy kappa))
	 (aone (translate-point x y (- rotation 90) radiusy))
	 (atwo (translate-point x y rotation radiusx))
	 (athree (translate-point x y (+ rotation 90) radiusy))
	 (afour (translate-point x y (+ rotation 180) radiusx))
	 (conea (translate-point (car aone) (cadr aone) (+ rotation 180) clengthx))
	 (coneb (translate-point (car aone) (cadr aone) rotation clengthx))
	 (ctwoa (translate-point (car atwo) (cadr atwo) (- rotation 90) clengthy))
	 (ctwob (translate-point (car atwo) (cadr atwo) (+ rotation 90) clengthy))
	 (cthreea (translate-point (car athree) (cadr athree) rotation clengthx))
	 (cthreeb (translate-point (car athree) (cadr athree) (+ rotation 180) clengthx))
	 (cfoura (translate-point (car afour) (cadr afour) (+ rotation 90) clengthy))
	 (cfourb (translate-point (car afour) (cadr afour) (- rotation 90) clengthy))
	 (strokeid (car (gimp-path-bezier-stroke-new-moveto pathvector (car aone) (cadr aone)))))
    (gimp-path-bezier-stroke-cubicto pathvector strokeid (car coneb) (cadr coneb) (car ctwoa) (cadr ctwoa) (car atwo) (cadr atwo))
    (gimp-path-bezier-stroke-cubicto pathvector strokeid (car ctwob) (cadr ctwob) (car cthreea) (cadr cthreea) (car athree) (cadr athree))
    (gimp-path-bezier-stroke-cubicto pathvector strokeid (car cthreeb) (cadr cthreeb) (car cfoura) (cadr cfoura) (car afour) (cadr afour))
    (gimp-path-bezier-stroke-cubicto pathvector strokeid (car cfourb) (cadr cfourb) (car conea) (cadr conea) (car aone) (cadr aone))
    (gimp-path-stroke-close pathvector strokeid)
    (gimp-image-insert-path img pathvector 0 -1)
		(make-vector-layer img pathvector fcolor scolor fillpath strokepath)
    (gimp-displays-flush)
  )
  (gimp-image-undo-group-end img))

(define (script-fu-oval-path img
			     drawable
			     name
			     x
			     y
			     radiustop
			     radiusleft
			     radiusbottom
			     radiusright
			     rotation
			     strokepath
			     scolor
			     fillpath
			     fcolor)
  (gimp-image-undo-group-start img)
  (let* ((pathvector (car (gimp-path-new img name)))
	 (top (- y radiustop))
	 (right (+ x radiusright))
	 (bottom (+ y radiusbottom))
	 (left (- x radiusleft))
	 (kappa (get-kappa))
	 (clength1 (* radiustop kappa))
	 (clength2 (* radiusright kappa))
	 (clength3 (* radiusbottom kappa))
	 (clength4 (* radiusleft kappa))
	 (aone (translate-point x y (- rotation 90) radiustop))
	 (atwo (translate-point x y rotation radiusright))
	 (athree (translate-point x y (+ rotation 90) radiusbottom))
	 (afour (translate-point x y (+ rotation 180) radiusleft))
	 (conea (translate-point (car aone) (cadr aone) (+ rotation 180) clength1))
	 (coneb (translate-point (car aone) (cadr aone) rotation clength1))
	 (ctwoa (translate-point (car atwo) (cadr atwo) (- rotation 90) clength2))
	 (ctwob (translate-point (car atwo) (cadr atwo) (+ rotation 90) clength2))
	 (cthreea (translate-point (car athree) (cadr athree) rotation clength3))
	 (cthreeb (translate-point (car athree) (cadr athree) (+ rotation 180) clength3))
	 (cfoura (translate-point (car afour) (cadr afour) (+ rotation 90) clength4))
	 (cfourb (translate-point (car afour) (cadr afour) (- rotation 90) clength4))
	 (strokeid (car (gimp-path-bezier-stroke-new-moveto pathvector (car aone) (cadr aone)))))
    (gimp-path-bezier-stroke-cubicto pathvector strokeid (car coneb) (cadr coneb) (car ctwoa) (cadr ctwoa) (car atwo) (cadr atwo))
    (gimp-path-bezier-stroke-cubicto pathvector strokeid (car ctwob) (cadr ctwob) (car cthreea) (cadr cthreea) (car athree) (cadr athree))
    (gimp-path-bezier-stroke-cubicto pathvector strokeid (car cthreeb) (cadr cthreeb) (car cfoura) (cadr cfoura) (car afour) (cadr afour))
    (gimp-path-bezier-stroke-cubicto pathvector strokeid (car cfourb) (cadr cfourb) (car conea) (cadr conea) (car aone) (cadr aone))
    (gimp-path-stroke-close pathvector strokeid)
    (gimp-image-insert-path img pathvector 0 -1)
		(make-vector-layer img pathvector fcolor scolor fillpath strokepath)
    (gimp-displays-flush)
  )
  (gimp-image-undo-group-end img))

(define (script-fu-rounded-rectangle-path img
					  drawable
					  name
					  top
					  left
					  bottom
					  right
					  radiusx
					  radiusy
					  strokepath
					  scolor
					  fillpath
					  fcolor)
  (gimp-image-undo-group-start img)
  (let* ((pathvector (car (gimp-path-new img name)))
	 (atop (+ top radiusy))
	 (aleft (+ left radiusx))
	 (abottom (- bottom radiusy))
	 (aright (- right radiusx))
	 (kappa (get-kappa))
	 (clengthx (* radiusx kappa))
	 (clengthy (* radiusy kappa))
	 (cup (- atop clengthy))
	 (cright (+ aright clengthx))
	 (cdown (+ abottom clengthy))
	 (cleft (- aleft clengthx))
	 (strokeid (car (gimp-path-bezier-stroke-new-moveto pathvector aleft top))))
    (gimp-path-bezier-stroke-lineto pathvector strokeid aright top)
    (gimp-path-bezier-stroke-cubicto pathvector strokeid cright top right cup right atop)
    (gimp-path-bezier-stroke-lineto pathvector strokeid right abottom)
    (gimp-path-bezier-stroke-cubicto pathvector strokeid right cdown cright bottom aright bottom)
    (gimp-path-bezier-stroke-lineto pathvector strokeid aleft bottom)
    (gimp-path-bezier-stroke-cubicto pathvector strokeid cleft bottom left cdown left abottom)
    (gimp-path-bezier-stroke-lineto pathvector strokeid left atop)
    (gimp-path-bezier-stroke-cubicto pathvector strokeid left cup cleft top aleft top)
    (gimp-path-stroke-close pathvector strokeid)
    (gimp-image-insert-path img pathvector 0 -1)
		(make-vector-layer img pathvector fcolor scolor fillpath strokepath)
    (gimp-displays-flush)
  )
  (gimp-image-undo-group-end img))

(define (script-fu-advanced-rounded-rectangle-path img
						   drawable
						   name
						   top
						   left
						   bottom
						   right
						   radiusx1
						   radiusy1
						   radiusx2
						   radiusy2
						   radiusx4
						   radiusy4
						   radiusx3
						   radiusy3
						   strokepath
						   scolor
						   fillpath
						   fcolor)
  (gimp-image-undo-group-start img)
  (let* ((pathvector (car (gimp-path-new img name)))
	 (atop (+ top radiusy1))
	 (aleft (+ left radiusx1))
	 (abottom (- bottom radiusy4))
	 (aright (- right radiusx2))
	 (atop2 (+ top radiusy2))
	 (aleft2 (+ left radiusx4))
	 (abottom2 (- bottom radiusy3))
	 (aright2 (- right radiusx3))
	 (kappa (get-kappa))
	 (cup (- atop (* radiusy1 kappa)))
	 (cright (+ aright (* radiusx2 kappa)))
	 (cdown (+ abottom (* radiusy4 kappa)))
	 (cleft (- aleft (* radiusx1 kappa)))
	 (cup2 (- atop2 (* radiusy2 kappa)))
	 (cright2 (+ aright2 (* radiusx3 kappa)))
	 (cdown2 (+ abottom2 (* radiusy3 kappa)))
	 (cleft2 (- aleft2 (* radiusx4 kappa)))
	 (strokeid (car (gimp-path-bezier-stroke-new-moveto pathvector aleft top))))
    (gimp-path-bezier-stroke-lineto pathvector strokeid aright top)
    (gimp-path-bezier-stroke-cubicto pathvector strokeid cright top right cup2 right atop2)
    (gimp-path-bezier-stroke-lineto pathvector strokeid right abottom2)
    (gimp-path-bezier-stroke-cubicto pathvector strokeid right cdown2 cright2 bottom aright2 bottom)
    (gimp-path-bezier-stroke-lineto pathvector strokeid aleft2 bottom)
    (gimp-path-bezier-stroke-cubicto pathvector strokeid cleft2 bottom left cdown left abottom)
    (gimp-path-bezier-stroke-lineto pathvector strokeid left atop)
    (gimp-path-bezier-stroke-cubicto pathvector strokeid left cup cleft top aleft top)
    (gimp-path-stroke-close pathvector strokeid)
    (gimp-image-insert-path img pathvector 0 -1)
	  (make-vector-layer img pathvector fcolor scolor fillpath strokepath)
    (gimp-displays-flush)
  )
  (gimp-image-undo-group-end img))

(define (script-fu-polygon-path img
				drawable
				name
				x
				y
				radius
				sides
				rotation
				strokepath
				scolor
				fillpath
				fcolor)
  (gimp-image-undo-group-start img)
  (let* ((pathvector (car (gimp-path-new img name)))
	 	 (init (- rotation 90))
	 (deg 0)
	 (i 1)
	 (p (translate-point x y init radius))
	 (strokeid (car (gimp-path-bezier-stroke-new-moveto pathvector (car p) (cadr p)))))
    (while (< i sides)
      (set! deg (+ (* (/ i sides) 360) init))
      (set! p (translate-point x y deg radius))
      (gimp-path-bezier-stroke-lineto pathvector strokeid (car p) (cadr p))
      (set! i (+ i 1))
    )
    (gimp-path-stroke-close pathvector strokeid)
    (gimp-image-insert-path img pathvector 0 -1)
		(make-vector-layer img pathvector fcolor scolor fillpath strokepath)
    (gimp-displays-flush)
  )
  (gimp-image-undo-group-end img))

(define (script-fu-star-path img
			     drawable
			     name
			     x
			     y
			     oradius
			     iradius
			     points
			     rotation
			     strokepath
			     scolor
			     fillpath
			     fcolor)
  (gimp-image-undo-group-start img)
  (let* ((pathvector (car (gimp-path-new img name)))
	 	 (pointstwo (* points 2))
	 (radius iradius)
	 (init (- rotation 90))
	 (deg 0)
	 (i 1)
	 (p (translate-point x y init oradius))
	 (strokeid (car (gimp-path-bezier-stroke-new-moveto pathvector (car p) (cadr p)))))
    (while (< i pointstwo)
      (set! deg (+ (* (/ i pointstwo) 360) init))
      (set! p (translate-point x y deg radius))
      (gimp-path-bezier-stroke-lineto pathvector strokeid (car p) (cadr p))
      (set! i (+ i 1))
      (if (= radius oradius)
	(set! radius iradius)
	(set! radius oradius)
      )
    )
    (gimp-path-stroke-close pathvector strokeid)
    (gimp-image-insert-path img pathvector 0 -1)
		(make-vector-layer img pathvector fcolor scolor fillpath strokepath)
    (gimp-displays-flush)
  )
  (gimp-image-undo-group-end img))

(define (script-fu-flowerspike-path img
				    drawable
				    name
				    x
				    y
				    radius
				    sides
				    rotation
				    factora
				    anga
				    factorb
				    angb
				    strokepath
				    scolor
				    fillpath
				    fcolor)
  (gimp-image-undo-group-start img)
  (let* ((pathvector (car (gimp-path-new img name)))
	 	 (init (- rotation 90))
	 (deg init)
	 (i 1)
	 (iradiusa (* radius factora))
	 (iradiusb (* radius factorb))
	 (p0 (translate-point x y init radius))
	 (p1 0)
	 (p2 0)
	 (p3 0)
	 (strokeid (car (gimp-path-bezier-stroke-new-moveto pathvector (car p0) (cadr p0)))))
    (while (< i sides)
      (set! p1 (translate-point x y (+ deg anga) iradiusa))
      (set! deg (+ (* (/ i sides) 360) init))
      (set! p2 (translate-point x y (+ deg angb) iradiusb))
      (set! p3 (translate-point x y deg radius))
      (gimp-path-bezier-stroke-cubicto pathvector strokeid (car p1) (cadr p1) (car p2) (cadr p2) (car p3) (cadr p3))
      (set! i (+ i 1))
    )
    (set! p1 (translate-point x y (+ deg anga) iradiusa))
    (set! deg init)
    (set! p2 (translate-point x y (+ deg angb) iradiusb))
    (gimp-path-bezier-stroke-cubicto pathvector strokeid (car p1) (cadr p1) (car p2) (cadr p2) (car p0) (cadr p0))
    (gimp-path-stroke-close pathvector strokeid)
    (gimp-image-insert-path img pathvector 0 -1)
		(make-vector-layer img pathvector fcolor scolor fillpath strokepath)
    (gimp-displays-flush)
  )
  (gimp-image-undo-group-end img))

(define (script-fu-gear-path img
			     drawable
			     name
			     x
			     y
			     oradius
			     iradius
			     teeth
			     rotation
			     strokepath
			     scolor
			     fillpath
			     fcolor)
  (gimp-image-undo-group-start img)
  (let* ((pathvector (car (gimp-path-new img name)))
	 	 (init (- rotation 90))
	 (deg 0)
	 (i 1)
	 (p (translate-point x y init iradius))
	 (strokeid (car (gimp-path-bezier-stroke-new-moveto pathvector (car p) (cadr p)))))
    (set! p (translate-point x y init oradius))
    (gimp-path-bezier-stroke-lineto pathvector strokeid (car p) (cadr p))
    (set! deg (+ (* (/ 0.5 teeth) 360) init))
    (set! p (translate-point x y deg oradius))
    (gimp-path-bezier-stroke-lineto pathvector strokeid (car p) (cadr p))
    (set! p (translate-point x y deg iradius))
    (gimp-path-bezier-stroke-lineto pathvector strokeid (car p) (cadr p))
    (while (< i teeth)
      (set! deg (+ (* (/ i teeth) 360) init))
      (set! p (translate-point x y deg iradius))
      (gimp-path-bezier-stroke-lineto pathvector strokeid (car p) (cadr p))
      (set! p (translate-point x y deg oradius))
      (gimp-path-bezier-stroke-lineto pathvector strokeid (car p) (cadr p))
      (set! deg (+ (* (/ (+ i 0.5) teeth) 360) init))
      (set! p (translate-point x y deg oradius))
      (gimp-path-bezier-stroke-lineto pathvector strokeid (car p) (cadr p))
      (set! p (translate-point x y deg iradius))
      (gimp-path-bezier-stroke-lineto pathvector strokeid (car p) (cadr p))
      (set! i (+ i 1))
    )
    (gimp-path-stroke-close pathvector strokeid)
    (gimp-image-insert-path img pathvector 0 -1)
		(make-vector-layer img pathvector fcolor scolor fillpath strokepath)
    (gimp-displays-flush)
  )
  (gimp-image-undo-group-end img))

(define (script-fu-triangle-wave-path img
				      drawable
				      name
				      x
				      y
				      amplitude
				      wavelength
				      cycles
				      shearing
				      rounding
				      strokepath
				      scolor)
  (gimp-image-undo-group-start img)
  (let* ((pathvector (car (gimp-path-new img name)))
	 	 (strokeid (car (gimp-path-bezier-stroke-new-moveto pathvector x y)))
	 (halfamplitude (/ amplitude 2))
	 (wavelength1 (/ wavelength 4))
	 (wavelength3 (* wavelength1 3))
	 (wavelength2 (* wavelength rounding))
	 (shear (* wavelength shearing))
	 (dist x)
	 (bottom (+ y halfamplitude))
	 (top (- y halfamplitude))
	 (distplusshearpluswl1 (+ dist shear wavelength1))
	 (distminusshearpluswl3 (+ (- dist shear) wavelength3))
	 (p1x x)
	 (p1y y)
	 (p2x 0)
	 (p2y 0)
	 (p3x 0)
	 (p3y 0)
	 (i 0))
    (while (< i cycles)
      (set! p2x (- distplusshearpluswl1 wavelength2))
      (set! p2y top)
      (set! p3x distplusshearpluswl1)
      (set! p3y top)
      (gimp-path-bezier-stroke-cubicto pathvector strokeid p1x p1y p2x p2y p3x p3y)
      (set! p1x (+ distplusshearpluswl1 wavelength2))
      (set! p1y top)
      (set! p2x (- distminusshearpluswl3 wavelength2))
      (set! p2y bottom)
      (set! p3x distminusshearpluswl3)
      (set! p3y bottom)
      (gimp-path-bezier-stroke-cubicto pathvector strokeid p1x p1y p2x p2y p3x p3y)
      (set! p1x (+ distminusshearpluswl3 wavelength2))
      (set! p1y bottom)
      (set! dist (+ dist wavelength))
      (set! distplusshearpluswl1 (+ dist shear wavelength1))
      (set! distminusshearpluswl3 (+ (- dist shear) wavelength3))
      (set! i (+ i 1))
    )
    (gimp-path-bezier-stroke-cubicto pathvector strokeid p1x p1y dist y dist y)
    (gimp-image-insert-path img pathvector 0 -1)
		(make-vector-layer img pathvector fcolor scolor fillpath strokepath)
    (gimp-displays-flush)
  )
  (gimp-image-undo-group-end img))

(define (script-fu-square-wave-path img
				    drawable
				    name
				    x
				    y
				    amplitude
				    wavelength
				    cycles
				    convergence
				    shearing
				    displacement
				    strokepath
				    scolor)
  (gimp-image-undo-group-start img)
  (let* ((pathvector (car (gimp-path-new img name)))
	 	 (strokeid (car (gimp-path-bezier-stroke-new-moveto pathvector x y)))
	 (halfamplitude (/ amplitude 2))
	 (halfwavelength (/ wavelength 2))
	 (dist x)
	 (convergence2  (* halfwavelength convergence))
	 (shearing2     (* halfwavelength shearing))
	 (displacement2 (* halfwavelength displacement))
	 (bottom (+ y halfamplitude))
	 (top (- y halfamplitude))
	 (px x)
	 (py y)
	 (i 0))
    (while (< i cycles)
      (set! px (- (+ dist shearing2) convergence2))
      (set! py top)
      (gimp-path-bezier-stroke-lineto pathvector strokeid px py)
      (set! dist (+ dist halfwavelength))
      (set! px (+ (+ (+ dist displacement2) convergence2) shearing2))
      (set! py top)
      (gimp-path-bezier-stroke-lineto pathvector strokeid px py)
      (set! px (- (- (+ dist displacement2) convergence2) shearing2))
      (set! py bottom)
      (gimp-path-bezier-stroke-lineto pathvector strokeid px py)
      (set! dist (+ dist halfwavelength))
      (set! px (+ (- dist shearing2) convergence2))
      (set! py bottom)
      (gimp-path-bezier-stroke-lineto pathvector strokeid px py)
      (set! i (+ i 1))
    )
    (gimp-path-bezier-stroke-lineto pathvector strokeid dist y)
    (gimp-image-insert-path img pathvector 0 -1)
		(make-vector-layer img pathvector fcolor scolor fillpath strokepath)
    (gimp-displays-flush)
  )
  (gimp-image-undo-group-end img))

(define (script-fu-sine-wave-path img
				  drawable
				  name
				  x
				  y
				  amplitude
				  wavelength
				  cycles
				  strokepath
				  scolor)
  (gimp-image-undo-group-start img)
  (let* ((pathvector (car (gimp-path-new img name)))
	 	 (strokeid (car (gimp-path-bezier-stroke-new-moveto pathvector x y)))
	 (rightone (/ wavelength 24))
	 (righttwo (/ wavelength 12))
	 (rightthree (/ wavelength 8))
	 (rightfour (/ wavelength 6))
	 (rightfive (/ wavelength 4.8))
	 (rightsix (/ wavelength 4))
	 (rightseven (/ wavelength (/ 12 3.5)))
	 (righteight (/ wavelength 3))
	 (rightnine (/ (* 3 wavelength) 8))
	 (rightten (/ wavelength (/ 12 5)))
	 (righteleven (/ wavelength (/ 12 5.5)))
	 (righttwelve (/ wavelength 2))
	 (rightthirteen (+ righttwelve rightone))
	 (rightfourteen (+ righttwelve righttwo))
	 (rightfifteen (+ righttwelve rightthree))
	 (rightsixteen (+ righttwelve rightfour))
	 (rightseventeen (+ righttwelve rightfive))
	 (righteighteen (+ righttwelve rightsix))
	 (rightnineteen (+ righttwelve rightseven))
	 (righttwenty (+ righttwelve righteight))
	 (righttwentyone (+ righttwelve rightnine))
	 (righttwentytwo (+ righttwelve rightten))
	 (righttwentythree (+ righttwelve righteleven))
	 (righttwentyfour (+ righttwelve righttwelve))
	 (righttwentyfive (+ righttwelve righttwelve rightone))
	 (yoveramp (/ y amplitude))
	 (oneseventh (/ 1 7))
	 (twosevenths (/ 2 7))
	 (rad2 (sqrt 2))
	 (twotimesrad2over7 (/ (* 2 rad2) 7))
	 (threetimesrad2over7 (/ (* 3 rad2) 7))
	 (fourtimesrad2over7 (/ (* 4 rad2) 7))
	 (halfrad2timesamp (* (/ rad2 2) amplitude))
	 (upone (* (+ (- yoveramp twotimesrad2over7) oneseventh) amplitude))
	 (uptwo (* (+ (- yoveramp fourtimesrad2over7) twosevenths) amplitude))
	 (upthree (+ (* halfrad2timesamp -1) y))
	 (upfour (* (- (- yoveramp threetimesrad2over7) twosevenths) amplitude))
	 (upfive (- y amplitude))
	 (downone (* (- (+ yoveramp twotimesrad2over7) oneseventh) amplitude))
	 (downtwo (* (- (+ yoveramp fourtimesrad2over7) twosevenths) amplitude))
	 (downthree (+ halfrad2timesamp y))
	 (downfour (* (+ (+ yoveramp threetimesrad2over7) twosevenths) amplitude))
	 (downfive (+ y amplitude))
	 (p1x (+ x rightone))
	 (p1y upone)
	 (p2x 0)
	 (p2y 0)
	 (p3x 0)
	 (p3y 0)
	 (i 0)
	 (distoffset 0))
    (while (< i cycles)
      (set! distoffset (* righttwentyfour i))
      (set! p2x (+ x distoffset righttwo))
      (set! p2y uptwo)
      (set! p3x (+ x distoffset rightthree))
      (set! p3y upthree)
      (gimp-path-bezier-stroke-cubicto pathvector strokeid p1x p1y p2x p2y p3x p3y)
      (set! p1x (+ x distoffset rightfour))
      (set! p1y upfour)
      (set! p2x (+ x distoffset rightfive))
      (set! p2y upfive)
      (set! p3x (+ x distoffset rightsix))
      (set! p3y upfive)
      (gimp-path-bezier-stroke-cubicto pathvector strokeid p1x p1y p2x p2y p3x p3y)
      (set! p1x (+ x distoffset rightseven))
      (set! p1y upfive)
      (set! p2x (+ x distoffset righteight))
      (set! p2y upfour)
      (set! p3x (+ x distoffset rightnine))
      (set! p3y upthree)
      (gimp-path-bezier-stroke-cubicto pathvector strokeid p1x p1y p2x p2y p3x p3y)
      (set! p1x (+ x distoffset rightten))
      (set! p1y uptwo)
      (set! p2x (+ x distoffset righteleven))
      (set! p2y upone)
      (set! p3x (+ x distoffset righttwelve))
      (set! p3y y)
      (gimp-path-bezier-stroke-cubicto pathvector strokeid p1x p1y p2x p2y p3x p3y)
      (set! p1x (+ x distoffset rightthirteen))
      (set! p1y downone)
      (set! p2x (+ x distoffset rightfourteen))
      (set! p2y downtwo)
      (set! p3x (+ x distoffset rightfifteen))
      (set! p3y downthree)
      (gimp-path-bezier-stroke-cubicto pathvector strokeid p1x p1y p2x p2y p3x p3y)
      (set! p1x (+ x distoffset rightsixteen))
      (set! p1y downfour)
      (set! p2x (+ x distoffset rightseventeen))
      (set! p2y downfive)
      (set! p3x (+ x distoffset righteighteen))
      (set! p3y downfive)
      (gimp-path-bezier-stroke-cubicto pathvector strokeid p1x p1y p2x p2y p3x p3y)
      (set! p1x (+ x distoffset rightnineteen))
      (set! p1y downfive)
      (set! p2x (+ x distoffset righttwenty))
      (set! p2y downfour)
      (set! p3x (+ x distoffset righttwentyone))
      (set! p3y downthree)
      (gimp-path-bezier-stroke-cubicto pathvector strokeid p1x p1y p2x p2y p3x p3y)
      (set! p1x (+ x distoffset righttwentytwo))
      (set! p1y downtwo)
      (set! p2x (+ x distoffset righttwentythree))
      (set! p2y downone)
      (set! p3x (+ x distoffset righttwentyfour))
      (set! p3y y)
      (gimp-path-bezier-stroke-cubicto pathvector strokeid p1x p1y p2x p2y p3x p3y)
      (set! p1x (+ x distoffset righttwentyfive))
      (set! p1y upone)
      (set! i (+ i 1))
    )
    (gimp-image-insert-path img pathvector 0 -1)
		(make-vector-layer img pathvector fcolor scolor fillpath strokepath)
    (gimp-displays-flush)
  )
  (gimp-image-undo-group-end img))

(define (script-fu-archimedean-spiral-path img
					drawable
					name
					x
					y
					turns
					space
					rotation
					strokepath
					scolor)
	(gimp-image-undo-group-start img)
	(let* ((pathvector (car (gimp-path-new img name)))
	 	 (strokeid (car (gimp-path-bezier-stroke-new-moveto pathvector x y)))
	 (init (- rotation 90))
	 (deg init)
	 (segmentsperturn 64)
	 (step (/ 1 segmentsperturn))
	 (i step)
	 (radius1 0)
	 (deg1 0)
	 (radius2 0)
	 (deg2 0)
	 (radius3 0)
	 (deg3 0)
	 (p0 (list x y))
	 (p1 0)
	 (p2 0)
	 (p3 0)
	 (t0 0)
	 (t1 0)
	 (t2 0)
	 (theta1 0)
	 (theta2 0)
	 (r1 0)
	 (r2 0)
	 (dx1 0)
	 (dy1 0)
	 (dx2 0)
	 (dy2 0)
	 (p (translate-point x y deg 0)))
		(while (< i turns)
			(set! deg1 (+ (* i 360) init))
			(set! radius1 (* space i))
			(set! t0 (- i step))
			(set! t1 (+ t0 (/ step 3)))
			(set! t2 (+ t0 (* 2 (/ step 3))))
			(set! theta1 (+ (* t1 360) init))
			(set! theta2 (+ (* t2 360) init))
			(set! r1 (* space t1))
			(set! r2 (* space t2))
			(set! p1 (translate-point x y theta1 r1))     ; p1_abs
			(set! p2 (translate-point x y theta2 r2))     ; p2_abs
			(set! p3 (translate-point x y deg1 radius1))  ; p3_abs
			(set! dx1 (- (car p1) (car p0)))
			(set! dy1 (- (cadr p1) (cadr p0)))
			(set! deg2 (* (atan dy1 dx1) (/ 180 (* 4 (atan 1.0)))))
			(set! radius2 (sqrt (+ (* dx1 dx1) (* dy1 dy1))))
			(set! dx2 (- (car p2) (car p3)))
			(set! dy2 (- (cadr p2) (cadr p3)))
			(set! deg3 (* (atan dy2 dx2) (/ 180 (* 4 (atan 1.0)))))
			(set! radius3 (sqrt (+ (* dx2 dx2) (* dy2 dy2))))
			(set! p3 (translate-point x y deg1 radius1))
			(set! p1 (translate-point (car p0) (cadr p0) deg2 radius2))
			(set! p2 (translate-point (car p3) (cadr p3) deg3 radius3))
			(gimp-path-bezier-stroke-cubicto pathvector strokeid (car p1) (cadr p1) (car p2) (cadr p2) (car p3) (cadr p3))
			(set! i (+ i step))
		  (set! p0 p3)
		)
		(gimp-image-insert-path img pathvector 0 -1)
		(make-vector-layer img pathvector (list 0 0 0) scolor FALSE strokepath)
		(gimp-displays-flush)
	)
	(gimp-image-undo-group-end img))

(script-fu-register-filter "script-fu-rectangle-path"
			   _"_Rectangle..."
			   "Creates a rectangular path."
			   "Jonathan Stipe"
			   "Jonathan Stipe"
			   "December 2004"
			   "*"
			   SF-ONE-DRAWABLE
			   SF-STRING	"Name"	"Rectangle"
			   SF-ADJUSTMENT	"Top"	'(0 0 65536 1 10 1 1)
			   SF-ADJUSTMENT	"Left"	'(0 0 65536 1 10 1 1)
			   SF-ADJUSTMENT	"Bottom"	'(10 0 65536 1 10 1 1)
			   SF-ADJUSTMENT	"Right"	'(10 0 65536 1 10 1 1)
			   SF-TOGGLE	"Stroke Path" FALSE
			   SF-COLOR	"Stroke Color" '(0 0 0)
			   SF-TOGGLE	"Fill Path" FALSE
			   SF-COLOR	"Fill Color" '(255 255 255))

(script-fu-menu-register "script-fu-rectangle-path"
                         "<Image>/Script-Fu/Shape Paths/")

(script-fu-register-filter "script-fu-square-path"
			   _"_Square..."
			   "Creates a square path."
			   "Jonathan Stipe"
			   "Jonathan Stipe"
			   "December 2004"
			   "*"
			   SF-ONE-DRAWABLE
			   SF-STRING	"Name"	"Square"
			   SF-ADJUSTMENT	"Top"	'(0 0 65536 1 10 1 1)
			   SF-ADJUSTMENT	"Left"	'(0 0 65536 1 10 1 1)
			   SF-ADJUSTMENT	"Size"	'(10 0 65536 1 10 1 1)
			   SF-TOGGLE	"Stroke Path" FALSE
			   SF-COLOR	"Stroke Color" '(0 0 0)
			   SF-TOGGLE	"Fill Path" FALSE
			   SF-COLOR	"Fill Color" '(255 255 255))

(script-fu-menu-register "script-fu-square-path"
                         "<Image>/Script-Fu/Shape Paths/")

(script-fu-register-filter "script-fu-circle-path"
			   _"_Circle..."
			   "Creates a circular path."
			   "Jonathan Stipe"
			   "Jonathan Stipe"
			   "December 2004"
			   "*"
			   SF-ONE-DRAWABLE
			   SF-STRING	"Name"	"Circle"
			   SF-ADJUSTMENT	"Center X"	'(0 0 65536 1 10 1 1)
			   SF-ADJUSTMENT	"Center Y"	'(0 0 65536 1 10 1 1)
			   SF-ADJUSTMENT	"Radius"	'(10 0 65536 1 10 1 1)
			   SF-TOGGLE	"Stroke Path" FALSE
			   SF-COLOR	"Stroke Color" '(0 0 0)
			   SF-TOGGLE	"Fill Path" FALSE
			   SF-COLOR	"Fill Color" '(255 255 255))

(script-fu-menu-register "script-fu-circle-path"
                         "<Image>/Script-Fu/Shape Paths/")

(script-fu-register-filter "script-fu-ellipse-path"
			   _"_Ellipse..."
			   "Creates an elliptical path."
			   "Jonathan Stipe"
			   "Jonathan Stipe"
			   "December 2004"
			   "*"
			   SF-ONE-DRAWABLE
			   SF-STRING	"Name"	"Ellipse"
			   SF-ADJUSTMENT	"Center X"	'(0 0 65536 1 10 1 1)
			   SF-ADJUSTMENT	"Center Y"	'(0 0 65536 1 10 1 1)
			   SF-ADJUSTMENT	"Horizontal Radius"	'(10 0 65536 1 10 1 1)
			   SF-ADJUSTMENT	"Vertical Radius"	'(10 0 65536 1 10 1 1)
			   SF-ADJUSTMENT "Rotation" '(0 0 360 1 10 1 0)
			   SF-TOGGLE	"Stroke Path" FALSE
			   SF-COLOR	"Stroke Color" '(0 0 0)
			   SF-TOGGLE	"Fill Path" FALSE
			   SF-COLOR	"Fill Color" '(255 255 255))

(script-fu-menu-register "script-fu-ellipse-path"
                         "<Image>/Script-Fu/Shape Paths/")

(script-fu-register-filter "script-fu-oval-path"
			   _"O_val..."
			   "Creates an ovoidal path with four radii."
			   "Jonathan Stipe and Pucelo"
			   "Jonathan Stipe and Pucelo"
			   "April 2007"
			   "*"
			   SF-ONE-DRAWABLE
			   SF-STRING	"Name"	"Oval"
			   SF-ADJUSTMENT	"Center X"	'(0 0 65536 1 10 1 1)
			   SF-ADJUSTMENT	"Center Y"	'(0 0 65536 1 10 1 1)
			   SF-ADJUSTMENT	"Top radius"	'(10 0 65536 1 10 1 1)
			   SF-ADJUSTMENT	"Left radius"	'(10 0 65536 1 10 1 1)
			   SF-ADJUSTMENT	"Bottom radius"	'(10 0 65536 1 10 1 1)
			   SF-ADJUSTMENT	"Right radius"	'(10 0 65536 1 10 1 1)
			   SF-ADJUSTMENT "Rotation" '(0 0 360 1 10 1 0)
			   SF-TOGGLE	"Stroke Path" FALSE
			   SF-COLOR	"Stroke Color" '(0 0 0)
			   SF-TOGGLE	"Fill Path" FALSE
			   SF-COLOR	"Fill Color" '(255 255 255))

(script-fu-menu-register "script-fu-oval-path"
                         "<Image>/Script-Fu/Shape Paths/")

(script-fu-register-filter "script-fu-rounded-rectangle-path"
			   _"R_ounded Rectangle..."
			   "Creates a rounded rectangular path."
			   "Jonathan Stipe"
			   "Jonathan Stipe"
			   "December 2004"
			   "*"
			   SF-ONE-DRAWABLE
			   SF-STRING	"Name"	"RoundRectangle"
			   SF-ADJUSTMENT	"Top"	'(0 0 65536 1 10 1 1)
			   SF-ADJUSTMENT	"Left"	'(0 0 65536 1 10 1 1)
			   SF-ADJUSTMENT	"Bottom"	'(10 0 65536 1 10 1 1)
			   SF-ADJUSTMENT	"Right"	'(10 0 65536 1 10 1 1)
			   SF-ADJUSTMENT	"Horizontal Radius"	'(0 0 65536 1 10 1 1)
			   SF-ADJUSTMENT	"Vertical Radius"	'(0 0 65536 1 10 1 1)
			   SF-TOGGLE	"Stroke Path" FALSE
			   SF-COLOR	"Stroke Color" '(0 0 0)
			   SF-TOGGLE	"Fill Path" FALSE
			   SF-COLOR	"Fill Color" '(255 255 255))

(script-fu-menu-register "script-fu-rounded-rectangle-path"
                         "<Image>/Script-Fu/Shape Paths/")

(script-fu-register-filter "script-fu-advanced-rounded-rectangle-path"
			   _"_Advanced Rounded Rectangle..."
			   "Creates a rounded rectangular path with different radii for each corner."
			   "Jonathan Stipe and Pucelo"
			   "Jonathan Stipe and Pucelo"
			   "April 2007"
			   "*"
			   SF-ONE-DRAWABLE
			   SF-STRING	"Name"	"AdvRoundRectangle"
			   SF-ADJUSTMENT	"Top"	'(0 0 65536 1 10 1 1)
			   SF-ADJUSTMENT	"Left"	'(0 0 65536 1 10 1 1)
			   SF-ADJUSTMENT	"Bottom"	'(10 0 65536 1 10 1 1)
			   SF-ADJUSTMENT	"Right"	'(10 0 65536 1 10 1 1)
			   SF-ADJUSTMENT	"Top-Left Horizontal Radius"	'(0 -65536 65536 1 10 1 1)
			   SF-ADJUSTMENT	"Top-Left Vertical Radius"	'(0 -65536 65536 1 10 1 1)
			   SF-ADJUSTMENT	"Top-Right Horizontal Radius"	'(0 -65536 65536 1 10 1 1)
			   SF-ADJUSTMENT	"Top-Right Vertical Radius"	'(0 -65536 65536 1 10 1 1)
			   SF-ADJUSTMENT	"Bottom-Left Horizontal Radius"	'(0 -65536 65536 1 10 1 1)
			   SF-ADJUSTMENT	"Bottom-Left Vertical Radius"	'(0 -65536 65536 1 10 1 1)
			   SF-ADJUSTMENT	"Bottom-Right Horizontal Radius"	'(0 -65536 65536 1 10 1 1)
			   SF-ADJUSTMENT	"Bottom-Right Vertical Radius"	'(0 -65536 65536 1 10 1 1)
			   SF-TOGGLE	"Stroke Path" FALSE
			   SF-COLOR	"Stroke Color" '(0 0 0)
			   SF-TOGGLE	"Fill Path" FALSE
			   SF-COLOR	"Fill Color" '(255 255 255))

(script-fu-menu-register "script-fu-advanced-rounded-rectangle-path"
                         "<Image>/Script-Fu/Shape Paths/")

(script-fu-register-filter "script-fu-polygon-path"
			   _"_Polygon..."
			   "Creates a polygonal path."
			   "Jonathan Stipe"
			   "Jonathan Stipe"
			   "December 2004"
			   "*"
			   SF-ONE-DRAWABLE
			   SF-STRING	"Name"	"Polygon"
			   SF-ADJUSTMENT	"Center X"	'(0 0 65536 1 10 1 1)
			   SF-ADJUSTMENT	"Center Y"	'(0 0 65536 1 10 1 1)
			   SF-ADJUSTMENT	"Radius"	'(0 0 65536 1 10 1 1)
			   SF-ADJUSTMENT	"Number of sides"	'(3 3 360 1 5 0 1)
			   SF-ADJUSTMENT "Rotation" '(0 0 360 1 10 1 0)
			   SF-TOGGLE	"Stroke Path" FALSE
			   SF-COLOR	"Stroke Color" '(0 0 0)
			   SF-TOGGLE	"Fill Path" FALSE
			   SF-COLOR	"Fill Color" '(255 255 255))

(script-fu-menu-register "script-fu-polygon-path"
                         "<Image>/Script-Fu/Shape Paths/")

(script-fu-register-filter "script-fu-star-path"
			   _"S_tar..."
			   "Creates a star-shaped path."
			   "Jonathan Stipe"
			   "Jonathan Stipe"
			   "December 2004"
			   "*"
			   SF-ONE-DRAWABLE
			   SF-STRING	"Name"	"Star"
			   SF-ADJUSTMENT	"Center X"	'(0 0 65536 1 10 1 1)
			   SF-ADJUSTMENT	"Center Y"	'(0 0 65536 1 10 1 1)
			   SF-ADJUSTMENT	"Outer Radius"	'(0 0 65536 1 10 1 1)
			   SF-ADJUSTMENT	"Inner Radius"	'(0 0 65536 1 10 1 1)
			   SF-ADJUSTMENT	"Number of points"	'(5 3 360 1 5 0 1)
			   SF-ADJUSTMENT "Rotation" '(0 0 360 1 10 1 0)
			   SF-TOGGLE	"Stroke Path" FALSE
			   SF-COLOR	"Stroke Color" '(0 0 0)
			   SF-TOGGLE	"Fill Path" FALSE
			   SF-COLOR	"Fill Color" '(255 255 255))

(script-fu-menu-register "script-fu-star-path"
                         "<Image>/Script-Fu/Shape Paths/")

(script-fu-register-filter "script-fu-flowerspike-path"
			   _"_Flowers & Spikes..."
			   "Creates a flower or spike shaped path, negative factor values create complex stars."
			   "Jonathan Stipe and Pucelo"
			   "Jonathan Stipe and Pucelo"
			   "December 2006"
			   "*"
			   SF-ONE-DRAWABLE
			   SF-STRING	"Name"	"FlowerSpike"
			   SF-ADJUSTMENT	"Center X"	'(0 0 65536 1 10 1 1)
			   SF-ADJUSTMENT	"Center Y"	'(0 0 65536 1 10 1 1)
			   SF-ADJUSTMENT	"Radius"	'(0 0 65536 1 10 1 1)
			   SF-ADJUSTMENT	"Number of sides"	'(3 3 360 1 5 0 1)
			   SF-ADJUSTMENT "Rotation" '(0 0 360 1 10 1 0)
			   SF-ADJUSTMENT	"Factor A"	'(1 -65536 65536 0.1 1 1 1)
			   SF-ADJUSTMENT "Angular Displacement A" '(0 -180 180 1 10 1 0)
			   SF-ADJUSTMENT	"Factor B"	'(1 -65536 65536 0.1 1 1 1)
			   SF-ADJUSTMENT "Angular Displacement B" '(0 -180 180 1 10 1 0)
			   SF-TOGGLE	"Stroke Path" FALSE
			   SF-COLOR	"Stroke Color" '(0 0 0)
			   SF-TOGGLE	"Fill Path" FALSE
			   SF-COLOR	"Fill Color" '(255 255 255))

(script-fu-menu-register "script-fu-flowerspike-path"
                         "<Image>/Script-Fu/Shape Paths/")

(script-fu-register-filter "script-fu-gear-path"
			   _"_Gear..."
			   "Creates a gear-shaped path."
			   "Jonathan Stipe and Pucelo"
			   "Jonathan Stipe and Pucelo"
			   "December 2006"
			   "*"
			   SF-ONE-DRAWABLE
			   SF-STRING	"Name"	"Gear"
			   SF-ADJUSTMENT	"Center X"	'(0 0 65536 1 10 1 1)
			   SF-ADJUSTMENT	"Center Y"	'(0 0 65536 1 10 1 1)
			   SF-ADJUSTMENT	"Outer Radius"	'(0 0 65536 1 10 1 1)
			   SF-ADJUSTMENT	"Inner Radius"	'(0 0 65536 1 10 1 1)
			   SF-ADJUSTMENT	"Number of teeth"	'(3 3 360 1 5 0 1)
			   SF-ADJUSTMENT "Rotation" '(0 0 360 1 10 1 0)
			   SF-TOGGLE	"Stroke Path" FALSE
			   SF-COLOR	"Stroke Color" '(0 0 0)
			   SF-TOGGLE	"Fill Path" FALSE
			   SF-COLOR	"Fill Color" '(255 255 255))

(script-fu-menu-register "script-fu-gear-path"
                         "<Image>/Script-Fu/Shape Paths/")

(script-fu-register-filter "script-fu-triangle-wave-path"
			   _"_Triangle Wave..."
			   "Creates a triangle wave-shaped path."
			   "Jonathan Stipe and Pucelo"
			   "Jonathan Stipe and Pucelo"
			   "April 2007"
			   "*"
			   SF-ONE-DRAWABLE
			   SF-STRING	"Name"	"TriangleWave"
			   SF-ADJUSTMENT	"Start X"	'(0 0 65536 1 10 1 1)
			   SF-ADJUSTMENT	"Start Y"	'(0 0 65536 1 10 1 1)
			   SF-ADJUSTMENT	"Amplitude"	'(1 0 65536 1 10 1 1)
			   SF-ADJUSTMENT	"Wavelength"	'(1 0 65536 1 10 1 1)
			   SF-ADJUSTMENT	"Number of cycles"	'(10 1 65536 1 5 0 1)
			   SF-ADJUSTMENT	"Shearing"	'(0 -65536 65536 0.05 0.2 3 1)
			   SF-ADJUSTMENT	"Rounding"	'(0 -65536 65536 0.05 0.2 3 1)
			   SF-TOGGLE	"Stroke Path" FALSE
			   SF-COLOR	"Stroke Color" '(0 0 0))

(script-fu-menu-register "script-fu-triangle-wave-path"
                         "<Image>/Script-Fu/Shape Paths/")

(script-fu-register-filter "script-fu-square-wave-path"
			   _"S_quare Wave..."
			   "Creates a square wave-shaped path."
			   "Jonathan Stipe and Pucelo"
			   "Jonathan Stipe and Pucelo"
			   "April 2007"
			   "*"
			   SF-ONE-DRAWABLE
			   SF-STRING	"Name"	"SquareWave"
			   SF-ADJUSTMENT	"Start X"	'(0 -65536 65536 1 10 1 1)
			   SF-ADJUSTMENT	"Start Y"	'(0 -65536 65536 1 10 1 1)
			   SF-ADJUSTMENT	"Amplitude"	'(1 0 65536 1 10 1 1)
			   SF-ADJUSTMENT	"Wavelength"	'(1 0 65536 1 10 1 1)
			   SF-ADJUSTMENT	"Number of cycles"	'(10 1 65536 1 5 0 1)
			   SF-ADJUSTMENT	"Convergence"	'(0 -65536 65536 0.05 0.2 3 1)
			   SF-ADJUSTMENT	"Shearing"	'(0 -65536 65536 0.05 0.2 3 1)
			   SF-ADJUSTMENT	"Displacement"	'(0 -1 1 0.05 0.2 3 1)
			   SF-TOGGLE	"Stroke Path" FALSE
			   SF-COLOR	"Stroke Color" '(0 0 0))

(script-fu-menu-register "script-fu-square-wave-path"
                         "<Image>/Script-Fu/Shape Paths/")

(script-fu-register-filter "script-fu-sine-wave-path"
			   _"S_ine Wave..."
			   "Creates a sine wave-shaped path."
			   "Jonathan Stipe"
			   "Jonathan Stipe"
			   "January 2005"
			   "*"
			   SF-ONE-DRAWABLE
			   SF-STRING	"Name"	"SineWave"
			   SF-ADJUSTMENT	"Start X"	'(0 0 65536 1 10 1 1)
			   SF-ADJUSTMENT	"Start Y"	'(0 0 65536 1 10 1 1)
			   SF-ADJUSTMENT	"Amplitude"	'(1 0 65536 1 10 1 1)
			   SF-ADJUSTMENT	"Wavelength"	'(1 0 65536 1 10 1 1)
			   SF-ADJUSTMENT	"Number of cycles"	'(1 1 65536 1 5 0 1)
			   SF-TOGGLE	"Stroke Path" FALSE
			   SF-COLOR	"Stroke Color" '(0 0 0))

(script-fu-menu-register "script-fu-sine-wave-path"
                         "<Image>/Script-Fu/Shape Paths/")

(script-fu-register-filter "script-fu-archimedean-spiral-path"
			   _"A_rchimedean Spiral..."
			   "Creates an Archimedean spiral-shaped path."
			   "Jonathan Stipe"
			   "Jonathan Stipe"
			   "March 2026"
			   "*"
			   SF-ONE-DRAWABLE
			   SF-STRING	"Name"	"ArchimedeanSpiral"
			   SF-ADJUSTMENT	"Center X"	'(0 0 65536 1 10 1 1)
			   SF-ADJUSTMENT	"Center Y"	'(0 0 65536 1 10 1 1)
			   SF-ADJUSTMENT	"Number of turns"	'(5 1 360 1 5 0 1)
			   SF-ADJUSTMENT	"Spacing"	'(10 1 65536 1 10 1 1)
			   SF-ADJUSTMENT	"Rotation"	'(0 -360 360 1 10 1 0)
			   SF-TOGGLE	"Stroke Path" FALSE
			   SF-COLOR	"Stroke Color" '(0 0 0))

(script-fu-menu-register "script-fu-archimedean-spiral-path"
                         "<Image>/Script-Fu/Shape Paths/")

