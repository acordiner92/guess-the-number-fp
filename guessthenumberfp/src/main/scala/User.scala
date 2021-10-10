sealed abstract case class User private (firstName: String, lastName: String)

object User {
  def make(firstName: String, lastName: String): Option[User] =
    if (firstName.length != 0 && lastName.length != 0) then
      Some(new User(firstName, lastName) {})
    else None
}
