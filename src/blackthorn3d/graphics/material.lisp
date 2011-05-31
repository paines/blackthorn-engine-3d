;;;; files (the "Software"), to deal in the Software without
;;;; restriction, including without limitation the rights to use, copy,
;;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;;; of the Software, and to permit persons to whom the Software is
;;;; furnished to do so, subject to the following conditions:
;;;;
;;;; The above copyright notice and this permission notice shall be
;;;; included in all copies or substantial portions of the Software.
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;;; DEALINGS IN THE SOFTWARE.
;;;;

(in-package :blackthorn3d-graphics)

;;;
;;; Materials
;;;

(defclass material ()
  ((shader
    :accessor mat-shader
    :initarg :shader
    :initform nil
    :documentation "A handle to the shader program, ie, what is
                    returned by make-shader")
   (ambient
    :initarg :ambient
    :initform #(0.0 0.0 0.0 1.0)
    :documentation "ambient color of the material: the color when there
                    is no lighting.  This should generally not be used.
                    Instead use lights' ambient.  Glow will (probably)
                    be implemented separately")
   (diffuse
    :initarg :diffuse
    :initform #(1.0 1.0 1.0 1.0)
    :documentation "The lambert reflectance of the material. multiplied by any
                    texture color")
   (specular
    :initarg :specular
    :initform #(0.0 0.0 0.0 1.0)
    :documentation "Specular color: the color of specular highlights for
                    specularity > 0.0")
   (shininess
    :initarg :shininess
    :initform 0.0
    :documentation "Higher specularity gives sharper highlights, lower
                   duller highlights, simulating more rough materials")
   (texture
    :accessor mat-texture
    :initarg :tex
    :initform nil
    :documentation "The texture handle that will be loaded. Will add
                    support for multi-textures later (??)")))

(defmethod use-material ((this material))
  "loads a material into opengl state"
  (with-slots (ambient diffuse specular shininess texture) this
    (when shader 
      (enable-shader shader))
    (when ambient 
      (gl:material :front :ambient ambient))
    (when diffuse 
      (gl:material :front :diffuse diffuse))
    (when specular 
      (gl:material :front :specular specular)
      (gl:material :front :shininess shininess))
    (when texture 
      (gl:bind-texture :texture-2d texture))))

(defmethod use-material ((this blt-material))
  (with-slots (ambient diffuse specular shininess textures) this
    (when ambient 
      (gl:material :front :ambient ambient))
    (when diffuse 
      (if (arrayp diffuse)
          (progn
        ;    (use-texture *default-texture*)
            (gl:material :front :diffuse diffuse))
          (progn
            (gl:material :front :diffuse +white+)
       ;     (use-texture diffuse)
            )))
    (when specular 
      (gl:material :front :specular specular)
      (gl:material :front :shininess shininess))))