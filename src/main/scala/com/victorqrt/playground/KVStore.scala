package com.victorqrt.playground

trait KVStore[F[_, _]] {

  def put[K, V](f: F[K, V])(k: K, v: V): F[K, V]

  def get[K, V](f: F[K, V])(k: K): Option[V]

  def values[K, V](f: F[K, V]): List[V]

  def getOrElse[K, V](f: F[K, V])(k: K, v: V): V =
    get(f)(k) getOrElse v
}

object KVStoreInstances {

  implicit def MapKVStore: KVStore[Map] =
    new KVStore[Map] {

      def put[K, V](m: Map[K, V])(k: K, v: V): Map[K, V] = m + (k -> v)

      def get[K, V](m: Map[K, V])(k: K): Option[V] = m get k

      def values[K, V](m: Map[K, V]): List[V] = m.values.toList
    }
}
