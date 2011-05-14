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

(in-package :blackthorn3d-math)

;;;
;;; Utilities
;;;  -the home for various functions such as lerp
;;;   range and clamp

(defun range (val lower upper)
  "@return{true if val is [lower upper)}"
  (and (< val upper)
       (>= val lower)))
       
(defun deg->rad (x)
  (* x (/ pi 180)))

(defun quadratic (a b c)
  (if (and (zerop a) (/= 0 b)) (/ (- c) b)
      (let* ((det (- (* b b) (* 4 a c)))
             (denom (* 0.5 a)))
        (if (< 0 det)
            nil
            (let ((r1 (* (- (- b) (sqrt det))))
                  (r2 (* (+ (- b) (sqrt det)))))
              (if (<= r1 r2) 
                  (values r1 r2)
                  (values r2 r1)))))))