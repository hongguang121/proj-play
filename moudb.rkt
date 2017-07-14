#lang racket
(require db)

(define USER "develop")
(define PASS "Develop_mysql!")
(define mydb   
  (mysql-connect #:user USER
                 #:password PASS
                 #:server "172.16.9.96"
                 #:port 3906
                 #:database "moumou"))

;;静态角色
(define static-role
  (query-rows mydb
              "SELECT role_id, role_name, role_note, role_code from static_role"))

;;角色数据库
(define static-user-role
  (query-rows mydb
              "SELECT id, user_id, user_phone, role_id, user_role_note from static_user_role"))

;;静态权限
(define static-permission
  (query-rows mydb
              "SELECT id, permission_code, permission_name, permission_action, permission_type,
               permission_parent, permission_note, permission_menu, permission_icon from static_permission"))

;;静态菜单 ;;;;不同于static-permission
(define static-menu
  (query-rows mydb
              "SELECT menu_code, menu_name, menu_desc, menu_parent_code, menu_order, menu_url from static_menu"))

;;添加用户(权限为0或1)
(define (insert-user user-id new-user)
  (let ((user (filter (λ (x) (= user-id (vector-ref x 0))) static-user-role)))
    (let ((user-auth (vector-ref (car user) 3)))
      (if (<= user-auth 2)
          "inserted"
          "no authority"))))

;;更新用户信息(权限大于1的用户只能更新自己的信息)
(define (update-user user-id content . update-user)
  (let ((user (filter (λ (x) (= user-id (vector-ref x 0))) static-user-role)))
    (let ((user-auth (vector-ref (car user) 3)))
      (if (<= user-auth 2)
          "updated"
          '更新自己的信息))))

;;删除用户(权限为0或1)
(define (delete-user user-id)
  (let ((user (filter (λ (x) (= user-id (vector-ref x 0))) static-user-role)))
    (let ((user-auth (vector-ref (car user) 3)))
      (if (<= user-auth 2)
          "delable"
          "no authority"))))

;;静态角色权限
(define static-role-permission
  (query-rows mydb
              "SELECT id, role_id, permission_id, role_permission_note from static_role_permission"))

;;过滤角色
;;;;role-id：角色id sheet-id：表单位置 sheet：表单
(define (filter-role role-id sheet-id sheet)
  (filter (λ (x) (= role-id (vector-ref x sheet-id))) sheet))

;;过滤角色权限
(define (role-permission role-id)
  (filter (λ (x) (= role-id (vector-ref x 1))) static-role-permission))

;;简化角色列表
(define simple-role
  (map (λ (x) (cons (vector-ref x 0) (vector-ref x 1))) static-role))

;;简化权限列表
(define simple-permission
  (map (λ (x) (cons (vector-ref x 0) (vector-ref x 2))) static-permission))

;;角色权限列表
(define role-permission-list
  (map (λ (x) (role-permission (car x))) simple-role))

;;查询权限
(define (check-role-permission role-id)
  (let ((rpl (map vector->list (flatten role-permission-list))))
    (let ((role (filter (λ (x) (= role-id (second x))) rpl)))
      (map (λ (x) (flatten (filter (λ (y) (= (third x) (car y))) simple-permission))) role))))


;;一个用户可以对应多个角色

;;角色分组
(define super-admin
  (filter-role 1 3 static-user-role)) ;;超级管理员

(define admin
  (filter-role 2 3 static-user-role)) ;;管理员

(define service
  (filter-role 6 3 static-user-role)) ;;客服

(define agent
  (filter-role 7 3 static-user-role)) ;;代理商

(define finance
  (filter-role 10 3 static-user-role)) ;;财务

(define business
  (filter-role 11 3 static-user-role)) ;;运营

(define agent-admin
  (filter-role 12 3 static-user-role)) ;;代理商管理员

(define agent-starlevel
  (filter-role 13 3 static-user-role)) ;;星级代理商

(define operate-activit
  (filter-role 14 3 static-user-role)) ;;运营活动

(define operate-manager
  (filter-role 15 3 static-user-role)) ;;运营主管

(define agent-finance-admin
  (filter-role 16 3 static-user-role)) ;;财务代理商

(define by-grouping
  (list super-admin
        admin
        service
        agent
        finance
        business
        agent-admin
        agent-starlevel
        operate-activit
        operate-manager
        agent-finance-admin))

(define (user-permission user-id)
  (let ((role-id (vector-ref (first (filter (λ (x) (= user-id (vector-ref x 0))) static-user-role)) 3)))
    (let ((auth (check-role-permission role-id)))
      (map (λ (x) (append x (filter (λ (y) (= (car x) (vector-ref y 0))) static-permission))) auth))))

(provide (all-defined-out))
