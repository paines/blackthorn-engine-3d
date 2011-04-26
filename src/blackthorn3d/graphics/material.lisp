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
    :initarg :shader)
   (ambient
    :initarg :ambient
    :initform #(0.0 0.0 0.0 1.0))
   (diffuse
    :initarg :diffuse
    :initform #(1.0 1.0 1.0 1.0))
   (specular
    :initarg :specular
    :initform #(0.0 0.0 0.0 1.0))
   (texture
    :accessor mat-texture
    :initarg :tex)))


(defmethod use-material ((this material))
  (with-slots (ambient diffuse specular texture) this
    (gl:material :front :ambient ambient)
    (gl:material :front :diffuse diffuse)
    (gl:material :front :specular specular)
    (gl:bind-texture :texture-2d texture)))