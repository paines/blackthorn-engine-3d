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

(defun clamp (val lower upper)
  "@return{(val > upper ? upper : (val < lower ? lower : val))}
   Assumes lower < upper.  Otherwise, returns upper"
  (min upper (max lower val)))

(defun lerp (s lower upper)
  "@return{the linear interpolation of s from lower to upper.}
   s should be [0 1]; will be clamped if not"
  (+ lower 
     (* (clamp s 0.0 1.0)
        (- upper lower))))

(defun range (val lower upper)
  "@return{true if val is [lower upper)}"
  (and (< val upper)
       (>= val lower)))