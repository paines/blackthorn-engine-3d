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

(defun quadratic (a b c max)
  (when (zerop a)
    (if (zerop b)
        (return-from quadratic nil)
        (let ((res (/ (- c) b)))
          (return-from quadratic
            (if (and (<= res max) (>= res 0.0))
                res
                nil)))))
  (let* ((det (- (sq b) (* 4 a c)))
         (denom (/ 1 (* 2 a))))
    (if (< det 0)
        nil
        (let* ((r1 (* (- (- b) (sqrt det)) denom))
               (r2 (* (+ (- b) (sqrt det)) denom))
               (rmin (min r1 r2))
               (rmax (max r1 r2)))
          #+disabled
          (if (<= rmin max)
              (values rmin rmax))
                                        ;#+disabled
          (if (and (>= rmin 0.0) (<= rmin max))
              (values rmin rmax)
              (if (and (>= rmax 0.0) (<= rmax max))
                  (values rmax rmin)))))))
