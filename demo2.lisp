;;;; -*- mode: Lisp -*-
;;;;
;;;; $Id$
;;;;
;;;; A Common Lisp version of the the Java Prevalyer demo2 example
;;;;
;;;; Copyright (C) 2003, 2004 Sven Van Caekenberghe, Beta Nine BVBA.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser General Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(in-package :cl-prevalence)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Domain Model
;;;
;;; In this demo, the domain model is particularly simple.  The root
;;; object of a model is an instance of the class BANK.  A bank has a
;;; set (represented as a hash table) of ACCOUNTS.  An account has an
;;; ACCOUNT NUMBER which is an integer.  Account numbers are allocated
;;; sequentially starting at 1.

(defclass bank ()
  ((accounts-by-number :accessor get-accounts-by-number :initform (make-hash-table :test 'eql))
   (next-account-number :accessor get-next-account-number :initform 1)))

;;; In addition to an account number, an account also has a HOLDER
;;; which is a string that has no other interpretation.  The holder of
;;; an account can be modified after the account has been created.
;;; The BALANCE of the account is modified as a result of deposits and
;;; withdrawals.  An account also has a TRANSACTION-HISTORY which is a
;;; list of objects of type ACCOUNT-ENTRY.  Notice that the
;;; transaction history of an account does not contain any
;;; transactions in the sense of prevalence transactions.
(defclass account ()
  ((number :accessor get-number :initarg :number :initform -1)
   (holder :accessor get-holder :initarg :holder :initform "Unspecified")
   (balance :accessor get-balance :initform 0)
   (transaction-history :accessor get-transaction-history :initform nil)))

(defmethod print-object ((account account) stream)
  (with-slots (number holder balance) account
    (format stream "#<ACCOUNT ~d '~a' $~d>" number holder balance)))

;;; Whenever the balance of an account is modified, an instance of
;;; ACCOUNT-ENTRY is pushed on the transaction history of the account.
(defclass account-entry ()
  ((amount :accessor get-amount :initarg :amount)
   (timestamp :accessor get-timestamp :initarg :timestamp)))

(defun date-time->string (universal-time)
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time universal-time)
    (format nil
	    "~d/~2,'0d/~2,'0d ~2,'0d:~2,'0d:~2,'0d"
	    year month date hour minute second)))

(defmethod print-object ((account-entry account-entry) stream)
  (with-slots (timestamp amount) account-entry
    (format stream "#<ACCOUNT-ENTRY ~a ~@d>" (date-time->string timestamp) amount)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Prevalence System
;;;
;;; A prevalence system is represented by an instance of the class
;;; CL-PREVALENCE:PREVALENCE-SYSTEM or a subclass of that class.  Such
;;; a system contains a collection of root objects which are objects
;;; that can not be reached from any other objects, and from which
;;; every object in the domain model can be reached.  The root objects
;;; are accessed through unique keys.  Keys are compared using EQ.
;;;
;;; In this demo, a BANK is a root object, whereas an ACCOUNT is not
;;; because an account can be found by searching the hash table of a
;;; bank.  In this demo, there is a single root object, and its key is
;;; the keyword :BANK.
;;;
;;; The model must not be modified directly.  Instead, all
;;; modifications to model objects must be done by using TRANSACTIONS.
;;; A transaction is an instance of the class
;;; CL-PREVALENCE:TRANSACTION.  Such an instance contains the name of
;;; a function to be called in order to modify the model objects, and
;;; the arguments to that function, except that the first argument
;;; that will be given to the function is the prevalence system, and
;;; that is not part of the arguments stored in the transaction.
;;; Instead, the prevalence system is supplied to the EXECUTE
;;; function, which takes a prevalence system and a transaction as
;;; arguments.  Transactions must be serializable so that they can be
;;; stored in a transaction log in a file.  For that reason, it is
;;; important to store the NAME of a function, rather than the
;;; function itself.  Also, the arguments to a transaction must be
;;; serializable for the same reason.

;;; A prevalence system is associated with a location which is a
;;; directory.  This directory contains the transaction logs and the
;;; snapshots of the system. 
(defparameter *bank-system-location* (pathname "/tmp/demo2-prevalence-system/"))

;;; This function is called as a result of calling EXECUTE on a
;;; transaction.  A transaction created using this function should not
;;; contain any arguments.  As a consequence, this function is called
;;; only with the prevalence system that was supplied to the EXECUTE
;;; function
(defun tx-create-bank (system)
  (setf (get-root-object system :bank) (make-instance 'bank)))

;;; Initialize the bank system.  A single bank is stored in the
;;; system, and this function checks whether a bank has already been
;;; stored, and if not, creates and stores a new instance of the class
;;; BANK.  A bank will already exist in the system if the system
;;; already existed as a combination of snapshots and transaction logs
;;; in the directory that was passed to MAKE-PREVALENCE-SYSTEM.  
(defun init-bank-system (system)
  (unless (get-root-object system :bank)
    (execute system (make-transaction 'tx-create-bank)))
  system)

;;; Create a prevalence system associated with a directory.  If there
;;; is already a system stored there, then restore it by deserializing
;;; it, which will recreate the system as it was after the last
;;; transaction that was executed.  If not, a new system will be
;;; created.  Make sure that either way, the system has a single BANK
;;; instance in it.  
(defvar *bank-system*
  (let ((system (make-prevalence-system *bank-system-location*)))
    (init-bank-system system)))

;;; This function is called as a result of calling EXECUTE on a
;;; transaction.  A transaction created using this function should
;;; contain a list of a single argument, namely the holder of the
;;; account to be created.  The holder is not interpreted in any way,
;;; and just stored in a slot of the account, but the object used to
;;; represent the holder should be serializable, because transactions
;;; must be serializable.
;;;
;;; Since the model in this demo contains a single bank, the bank is
;;; implicit so it is not an argument to this transaction.  Instead
;;; the only bank object is found in the system as a root object with
;;; the key :BANK.  A new account number is allocated by taking the
;;; next available account number stored in the bank object.  
;;;
;;; The new account is returned by this function, but it is also
;;; stored in the set of all accounts in the bank object.  That set is
;;; a hash table with the account number as a key. 
(defun tx-create-account (system holder)
  (let* ((bank (get-root-object system :bank))
	 (account-number (get-next-account-number bank))
	 (new-account (make-instance 'account
				     :holder holder
				     :number account-number)))
    (setf (gethash account-number (get-accounts-by-number bank)) new-account) 
    (incf (get-next-account-number bank))
    new-account))

(define-condition bank-error (error) ())

(define-condition unknown-account (bank-error)
  ((account-number :reader unknown-account-number :initarg :account-number))
  (:report (lambda (condition stream)
	     (format stream "Unknown account ~a"
		     (unknown-account-number condition)))))

(define-condition overdrawn-account (bank-error)
  ((account :reader overdrawn-account-account :initarg :account)
   (amount :reader overdrawn-account-amount :initarg :amount))
  (:report (lambda (condition stream)
	     (format stream "You cannot withdraw ~d from account ~a"
		     (overdrawn-account-amount condition)
		     (overdrawn-account-account condition)))))

;;; This function is a helper function to be used in several
;;; transactions.  It takes a prevalence system containing a single
;;; bank object and an account number, and returns the account with
;;; that number in the bank object.  An error is signaled when there
;;; is no account with that number in the bank, so this function is
;;; also used just to check that the account exists. 
(defun get-account (system account-number)
  (let* ((bank (get-root-object system :bank))
	 (account (gethash account-number (get-accounts-by-number bank))))
    (if account
	account
      (error 'unknown-account :account-number account-number))))

;;; This function is called as a result of calling EXECUTE on a
;;; transaction.  A transaction created using this function should
;;; contain a list of a single argument, namely the account number of
;;; the account that should be deleted.  The function GET-ACCOUNT is
;;; called just to check that the account exists (an error is signaled
;;; otherwise).  This function works by finding the single bank in the
;;; prevalence system, then it accesses the set of all the accounts in
;;; that bank (which is a hash table with the account number as a
;;; key), and finally it removes that account from the set of all
;;; accounts.
(defun tx-delete-account (system account-number)
  (when (get-account system account-number)
    (remhash account-number (get-accounts-by-number (get-root-object system :bank)))))

(defun tx-change-account-holder (system account-number new-holder)
  (let ((account (get-account system account-number)))
    (setf (get-holder account) new-holder)
    account))

(defun tx-deposit (system account-number amount timestamp)
  (let ((account (get-account system account-number)))
    (incf (get-balance account) amount)
    (push (make-instance 'account-entry :amount amount :timestamp timestamp)
	  (get-transaction-history account))
    account))

(defun tx-withdraw (system account-number amount timestamp)
  (let ((account (get-account system account-number)))
    (if (< (get-balance account) amount)
	(error 'overdrawn-account :account account :amount amount)
      (decf (get-balance account) amount))
    (push (make-instance 'account-entry :amount (- amount) :timestamp timestamp)
	  (get-transaction-history account))
    account))

(defun tx-transfer (system from-account-number to-account-number amount timestamp)
  (let* ((from-account (get-account system from-account-number))
	 (to-account (get-account system to-account-number)))
    (cond ((< (get-balance from-account) amount)
	   (error 'overdrawn-account :amount amount :account from-account))
	  (t (decf (get-balance from-account) amount)
	     (incf (get-balance to-account) amount)
	     (push (make-instance 'account-entry :amount (- amount) :timestamp timestamp)
		   (get-transaction-history from-account))
	     (push (make-instance 'account-entry :amount amount :timestamp timestamp)
		   (get-transaction-history to-account))))
    amount))

(defun get-bank-balance (system)
  (let ((bank (get-root-object system :bank))
	(total 0))
    (maphash #'(lambda (key value)
		 (declare (ignore key))
		 (incf total (get-balance value)))
	     (get-accounts-by-number bank))
    total))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Client Interface
;;;
;;; This is a collection of functions to be used by application (or
;;; client) code.  Such code is not allowed to modify model objects
;;; directly.  Instead it does so indirectly by creating transactions
;;; that can be logged.

(defun create-account (holder)
  (execute *bank-system* (make-transaction 'tx-create-account holder)))

(defun delete-account (account-number)
  (execute *bank-system* (make-transaction 'tx-delete-account account-number)))

(defun change-account-holder (account-number new-holder)
  (execute *bank-system* (make-transaction 'tx-change-account-holder account-number new-holder)))

(defun deposit (account-number amount)
  (execute *bank-system* (make-transaction 'tx-deposit
					   account-number amount (get-universal-time))))

(defun withdraw (account-number amount)
  (execute *bank-system* (make-transaction 'tx-withdraw
					   account-number amount (get-universal-time))))

(defun transfer (from-account-number to-account-number amount)
  (execute *bank-system* (make-transaction 'tx-transfer
					   from-account-number to-account-number amount (get-universal-time))))

;;; Client code that does not make any modifications to model objects
;;; are not required to use transactions.  
(defun find-account (account-number)
  (let ((bank (get-root-object *bank-system* :bank)))
    (gethash account-number (get-accounts-by-number bank))))

(defun list-all-accounts ()
  (let ((bank (get-root-object *bank-system* :bank))
	accounts)
    (maphash #'(lambda (key value)
		 (declare (ignore key))
		 (push value accounts))
	     (get-accounts-by-number bank))
    accounts))

;;; Now some test code

(defun bank-test-1 ()
  (let ((test-1 (get-number (create-account "Test Account 1")))
	(test-2 (get-number (create-account "Test Account 2"))))
    (assert (zerop (get-balance (find-account test-1))))
    (assert (zerop (get-balance (find-account test-2))))
    (deposit test-1 1000)
    (deposit test-2 2000)
    (withdraw test-2 500)
    (transfer test-2 test-1 500)
    (withdraw test-1 500)
    (assert (= 1000 (get-balance (find-account test-1))))
    (assert (= 1000 (get-balance (find-account test-2))))
    (delete-account test-1)
    (delete-account test-2)
    (print-transaction-log *bank-system*)
    (snapshot *bank-system*)
    (print-snapshot *bank-system*)))

(defun bank-test-2 ()
  (let ((test-1 (get-number (create-account "Test Account 1")))
	(test-2 (get-number (create-account "Test Account 2")))
	now)
    (assert (zerop (get-balance (find-account test-1))))
    (assert (zerop (get-balance (find-account test-2))))
    (deposit test-1 1000)
    (deposit test-2 2000)
    (withdraw test-2 500)
    (transfer test-2 test-1 500)
    (withdraw test-1 500)
    (assert (= 1000 (get-balance (find-account test-1))))
    (assert (= 1000 (get-balance (find-account test-2))))
    (sleep 1)
    (setf now (get-universal-time))
    (restore *bank-system*)
    (let ((account-1 (find-account test-1))
	  (account-2 (find-account test-2)))
      (dolist (account-entry (get-transaction-history account-1))
	(assert (< (get-timestamp account-entry) now)))
      (dolist (account-entry (get-transaction-history account-2))
	(assert (< (get-timestamp account-entry) now))))
    (delete-account test-1)
    (delete-account test-2))
  t)

(defun bank-test-3 ()
  (let ((system (make-prevalence-system *bank-system-location*
					:prevalence-system-class 'guarded-prevalence-system)))
    (query system #'get-bank-balance)
    (close-open-streams system)))

(defmethod initiates-rollback ((bank-error bank-error))
  nil)

(defun tx-bogus-withdraw (system account-number amount)
  (let* ((bank (get-root-object system :bank))
	 (account (gethash account-number (get-accounts-by-number bank))))
    (if (null account)
	(error 'unknown-account :account-number account-number)
      (progn
	;; this is intentionally wrong: we modify before we test
	(decf (get-balance account) amount)
	;; if negative throw a hard error (could initiate rollback)
	(when (< (get-balance account) 0)
	  (error "Account ~a went below zero!" account))))))

(defun bank-test-4 ()
  (let ((account-number (get-number (create-account "bank-test4"))))
    ;; --------------------------------------------------------------
    (format t "Part 1~%")
    ;; disable the rollback option (off by default)
    (setf (get-option *bank-system* :rollback-on-error) nil)
    ;; put 10 bucks on the account
    (deposit account-number 10)
    ;; check that we have 10 bucks
    (assert (= 10 (get-balance (find-account account-number))))
    ;; try to withdraw 20 bucks from the account
    (ignore-errors
      ;; this will fail with an overdrawn-account error
      ;; BEFORE the system is modified (nothing was logged)
      (withdraw account-number 20))
    ;; check that nothing changed
    (assert (= 10 (get-balance (find-account account-number))))
    ;; try to with withdraw 20 bucks using the bogus-withdraw tx
    (ignore-errors
      ;; this will fail with a regular error
      ;; AFTER the system is modified (nothing was logged)
      (execute *bank-system* (make-transaction 'tx-bogus-withdraw account-number 20)))
    ;; check that the change went through
    (assert (= -10 (get-balance (find-account account-number))))
    ;; --------------------------------------------------------------
    (format t "Part 2~%")
    ;; enable the rollback option (off by default)
    (setf (get-option *bank-system* :rollback-on-error) t)
    ;; start over
    (delete-account account-number)
    (setf account-number (get-number (create-account "bank-test4")))
    ;; put 20 bucks on the account
    (deposit account-number 10)
    ;; check that we have 10 bucks 
    (assert (= 10 (get-balance (find-account account-number))))
    ;; try to withdraw 20 bucks from the account
    (ignore-errors
      ;; this will fail with an overdrawn-account error
      ;; BEFORE the system is modified (nothing was logged)
      ;; NO rollback (condition does not initiate a rollback)
      (withdraw account-number 20))
    ;; check that nothing changed
    (assert (= 10 (get-balance (find-account account-number))))
    ;; try to with withdraw 20 bucks using the bogus-withdraw tx
    (ignore-errors
      ;; this will fail with a regular error
      ;; AFTER the system is modified (nothing was logged)
      ;; rollback IS executed (condition does initiate a rollback)
      (execute *bank-system* (make-transaction 'tx-bogus-withdraw account-number 20)))
    ;; check that the rollback took place and nothing changed
    (assert (= 10 (get-balance (find-account account-number))))
    ;; --------------------------------------------------------------
    ;; reset
    (delete-account account-number)
    (setf (get-option *bank-system* :rollback-on-error) nil)))

;;; a multi-processing example

(defparameter *bank-system-lock*
  (make-process-lock "bank-system-lock"))

(defun bank-system-guard (thunk)
  (with-process-lock (*bank-system-lock*) (funcall thunk)))

(defun spawn-process (name function)
  (run-process name function))

(defun bank-test-5-setup ()
  (when *bank-system* (close-open-streams *bank-system*))
  (setf *bank-system* (make-prevalence-system *bank-system-location*
					      :prevalence-system-class 'guarded-prevalence-system))
  (setf (get-guard *bank-system*) #'bank-system-guard)
  (mapcar #'(lambda (account)
	      (delete-account (get-number account)))
	  (list-all-accounts))
  (dotimes (i 10)
    (deposit (get-number (create-account (format nil "bank-test-5-account-~d" i))) 100))
  (assert (= (get-bank-balance *bank-system*) 1000)))

(defparameter *worker-output* *standard-output*)

(defun bank-test-5-worker ()
  (dotimes (i 10)
    (let* ((accounts (list-all-accounts))
	   (from-account (elt accounts (random (length accounts))))
	   (to-account (elt (remove from-account accounts) (random (1- (length accounts)))))
	   (amount (random 100)))
      (catch 'trap-overdraw
	(handler-bind ((overdrawn-account (lambda (condition)
					    (format t "Transfer cancelled (~a)~%" condition)
					    (throw 'trap-overdraw :ignore))))
	  (format *worker-output* "Tranfering ~d from ~a to ~a~%" amount from-account to-account)
	  (transfer (get-number from-account)
		    (get-number to-account)
		    amount))))))

(defun bank-test-5-invariant ()
  (dotimes (i 10)
    (assert (= (query *bank-system* 'get-bank-balance) 1000))))

(defun bank-test-5 ()
  (bank-test-5-setup)
  (spawn-process "invariant" #'bank-test-5-invariant)
  (dotimes (i 10)
    (spawn-process (format nil "bank-test-5-worker-~d" i)
		   #'bank-test-5-worker)
    (spawn-process "invariant" #'bank-test-5-invariant))
  (spawn-process "invariant" #'bank-test-5-invariant)
  (sleep 1)
  (spawn-process "invariant" #'bank-test-5-invariant))
  
;;;; eof